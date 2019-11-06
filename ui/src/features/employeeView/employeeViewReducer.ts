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
  loadingSummary: boolean;
  loadingRequests: boolean;
  loadingVacations: boolean;
  addingRequest: boolean;
}

const initialState: State = {
  summary: undefined,
  requests: [],
  vacations: emptyVacations,
  currentRequest: '',
  loadingSummary: false,
  loadingRequests: false,
  loadingVacations: false,
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
    setField: (state: State, action: PayloadAction<Partial<State>>) => ({...state, ...action.payload}),
  },
});

const {
  setSummary,
  setRequests,
  setVacations,
  startAddRequest,
  endAddRequest,
  setField,
} = slice.actions;

export const {
  clearAll,
  setCurrentRequest,
} = slice.actions;


export const reducer = slice.reducer;

const loadSummary = (): AppThunk => async (dispatch, getState) => {
  try {
    dispatch(setField({loadingSummary: true}));
    const ledger = getLedger(getState());
    const key = {employeeRole: {employee: ledger.party}};
    const {argument: {employeeRole: {employee, boss}, remainingDays}} =
      await ledger.pseudoFetchByKey(DAVL.EmployeeVacationAllocation, key);
    const summary: EmployeeSummary = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    }
    dispatch(setSummary(summary));
  } finally {
    dispatch(setField({loadingSummary: false}));
  }
}

const loadRequests = (): AppThunk => async (dispatch, getState) => {
  try {
    dispatch(setField({loadingRequests: true}));
    const ledger = getLedger(getState());
    const requestsContracts =
      await ledger.query(DAVL.VacationRequest, {vacation: {employeeRole: {employee: ledger.party}}});
    const requests: Vacation[] =
      requestsContracts.map(({contractId, argument}) => makeVacation(contractId, argument.vacation));
    requests.sort(ordVacationOnFromDate.compare);
    dispatch(setRequests(requests));
  } finally {
    dispatch(setField({loadingRequests: false}));
  }
}

const loadVacations = (): AppThunk => async (dispatch, getState) => {
  try {
    dispatch(setField({loadingVacations: true}));
    const ledger = getLedger(getState());
    const vacationContracts =
      await ledger.query(DAVL.Vacation, {employeeRole: {employee: ledger.party}});
    const vacations: Vacation[] =
      vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.argument));
    dispatch(setVacations(splitVacations(vacations)));
  } finally {
    dispatch(setField({loadingVacations: false}));
  }
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
    const ledger = getLedger(getState());
    const key = {employee: ledger.party};
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
