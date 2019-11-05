import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import * as DAVL from '../../daml/DAVL';
import { Vacation, Vacations, makeVacation, ordVacationOnFromDate, emptyVacations, splitVacations } from '../../utils/vacation';
import { EmployeeSummary } from '../../utils/employee';
import { toast } from 'react-semantic-toasts';

export type State = {
  summary?: EmployeeSummary;
  requests: Vacation[];
  vacations: Vacations;
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  requests: [],
  vacations: emptyVacations,
  currentRequest: '',
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    clearAll: () => initialState,
    setSummary: (state: State, action: PayloadAction<EmployeeSummary>) => ({...state, summary: action.payload}),
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
    setVacations: (state: State, action: PayloadAction<Vacations>) => ({...state, vacations: action.payload}),
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State) => ({...state, addingRequest: true}),
    endAddRequest: (state: State) => ({...state, currentRequest: '', addingRequest: false}),
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
  const {argument: {employeeRole: {employee, boss}, remainingDays}} =
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
    vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.argument));
  dispatch(setVacations(splitVacations(vacations)));
}

const loadRequests = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const requestsContracts =
    await ledger.query(DAVL.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
  const requests: Vacation[] =
    requestsContracts.map(({contractId, argument}) => makeVacation(contractId, argument.vacation));
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
  try {
    dispatch(startAddRequest());
    const ledger = getLedger(getState);
    const key = {employee: ledger.party()};
    await ledger.pseudoExerciseByKey(DAVL.EmployeeRole.RequestVacation, key, {fromDate, toDate});
  } finally {
    dispatch(endAddRequest());
  }
  toast({
    title:' Success',
    type: 'success',
    description: 'Request successfully submitted.',
    time: 3000,
  });
  await dispatch(loadRequests());
}
