import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import * as DAVL from '../../daml/DAVL';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';
import { EmployeeSummary } from './types';
import moment from 'moment';
import { partition } from 'fp-ts/lib/Array';
import { getDualOrd } from 'fp-ts/lib/Ord';
import { toast } from 'react-semantic-toasts';

type Vacations = {
  upcoming: Vacation[];
  past: Vacation[];
}

export type State = {
  summary?: EmployeeSummary;
  requests: Vacation[];
  vacations: Vacations;
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  requests: [],
  vacations: {upcoming: [], past: []},
  currentRequest: '',
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setSummary: (state: State, action: PayloadAction<EmployeeSummary>) => ({...state, summary: action.payload}),
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
    setVacations: (state: State, action: PayloadAction<Vacations>) => ({...state, vacations: action.payload}),
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State, action: PayloadAction) => ({...state, addingRequest: true}),
    endAddRequest: (state: State, action: PayloadAction) => ({...state, currentRequest: '', addingRequest: false}),
  },
});

const {
  setSummary,
  setRequests,
  setVacations,
  startAddRequest,
  endAddRequest,
} = slice.actions;

export const {
  clearAll,
  setCurrentRequest,
} = slice.actions;


export const reducer = slice.reducer;

const loadSummary = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const key = {employeeRole: {employee: ledger.party()}};
  const {data: {employeeRole: {employee, boss}, remainingDays}} =
    await ledger.pseudoFetchByKey(DAVL.EmployeeVacationAllocation, key);
  const summary: EmployeeSummary = {
    employee,
    boss,
    remainingVacationDays: remainingDays,
  }
  dispatch(setSummary(summary));
}

const loadVacations = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const vacationContracts =
    await ledger.query(DAVL.Vacation, {employeeRole: {employee: ledger.party()}});
  const vacations: Vacation[] =
    vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
  const today = moment().format('YYYY-MM-DD');
  const {left: upcoming, right: past} =
    partition((vacation: Vacation) => vacation.fromDate <= today)(vacations);
  upcoming.sort(ordVacationOnFromDate.compare);
  past.sort(getDualOrd(ordVacationOnFromDate).compare);
  dispatch(setVacations({upcoming, past}));
}

const loadRequests = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const requestsContracts =
    await ledger.query(DAVL.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
  const requests: Vacation[] =
    requestsContracts.map(({contractId, data}) => makeVacation(contractId, data.vacation));
  requests.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(requests));
}

export const loadAll = (): AppThunk => async (dispatch) => {
  await Promise.all([
    dispatch(loadSummary()),
    dispatch(loadVacations()),
    dispatch(loadRequests()),
  ]);
}

export const addRequest = (fromDate: string, toDate: string): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  dispatch(startAddRequest());
  const key = {employee: ledger.party()};
  await ledger.pseudoExerciseByKey(DAVL.EmployeeRole.RequestVacation, key, {fromDate, toDate});
  dispatch(endAddRequest());
  toast({
    title:' Success',
    type: 'success',
    description: 'Request successfully submitted.',
    time: 3000,
  });
  await dispatch(loadRequests());
}
