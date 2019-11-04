import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import * as DAVL from '../../daml/DAVL';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';
import { EmployeeSummary } from './types';
import moment from 'moment';
import { partition } from 'fp-ts/lib/Array';
import { getDualOrd } from 'fp-ts/lib/Ord';

export type State = {
  summary?: EmployeeSummary;
  pending: Vacation[];
  upcomingVacations: Vacation[];
  pastVacations: Vacation[];
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  upcomingVacations: [],
  pastVacations: [],
  pending: [],
  currentRequest: '',
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setSummary: (state: State, action: PayloadAction<EmployeeSummary>) => ({...state, summary: action.payload}),
    setPending: (state: State, action: PayloadAction<Vacation[]>) => ({...state, pending: action.payload}),
    setUpcomingVacations: (state: State, action: PayloadAction<Vacation[]>) => ({...state, upcomingVacations: action.payload}),
    setPastVacations: (state: State, action: PayloadAction<Vacation[]>) => ({...state, pastVacations: action.payload}),
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State, action: PayloadAction) => ({...state, addingRequest: true}),
    endAddRequest: (state: State, action: PayloadAction) => ({...state, currentRequest: '', addingRequest: false}),
  },
});

const {
  setSummary,
  setPending,
  setUpcomingVacations,
  setPastVacations,
  startAddRequest,
  endAddRequest,
} = slice.actions;

export const {
  clearAll,
  setCurrentRequest,
} = slice.actions;


export const reducer = slice.reducer;

const loadSummary = (ledger: Ledger): AppThunk => async (dispatch) => {
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

const loadVacations = (ledger: Ledger): AppThunk => async (dispatch) => {
  const vacationContracts =
    await ledger.query(DAVL.Vacation, {employeeRole: {employee: ledger.party()}});
  const vacations: Vacation[] =
    vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
  const today = moment().format('YYYY-MM-DD');
  const {left: upcomingVacations, right: pastVacations} =
    partition((vacation: Vacation) => vacation.fromDate <= today)(vacations);
  upcomingVacations.sort(ordVacationOnFromDate.compare);
  pastVacations.sort(getDualOrd(ordVacationOnFromDate).compare);
  dispatch(setUpcomingVacations(upcomingVacations));
  dispatch(setPastVacations(pastVacations));
}

const loadRequests = (ledger: Ledger): AppThunk => async (dispatch) => {
  const requestsContracts =
    await ledger.query(DAVL.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
  const requests: Vacation[] =
    requestsContracts.map(({contractId, data}) => makeVacation(contractId, data.vacation));
  requests.sort(ordVacationOnFromDate.compare);
  dispatch(setPending(requests));
}

export const loadAll = (ledger: Ledger): AppThunk => async (dispatch) => {
  await Promise.all([
    dispatch(loadSummary(ledger)),
    dispatch(loadVacations(ledger)),
    dispatch(loadRequests(ledger)),
  ]);
}

export const addRequest = (ledger: Ledger, fromDate: string, toDate: string): AppThunk => async (dispatch) => {
  dispatch(startAddRequest());
  const key = {employee: ledger.party()};
  await ledger.pseudoExerciseByKey(DAVL.EmployeeRole.RequestVacation, key, {fromDate, toDate});
  dispatch(endAddRequest());
  await dispatch(loadRequests(ledger));
}
