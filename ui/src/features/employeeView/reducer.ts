import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import * as davl from '../../daml/DAVL';
import { Vacation, makeVacation } from '../../utils/vacation';
import { EmployeeSummary } from './types';

export type State = {
  summary: EmployeeSummary | null;
  pending: Vacation[];
  approved: Vacation[];
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  summary: null,
  approved: [],
  pending: [],
  currentRequest: '',
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    setSummary: (state: State, action: PayloadAction<EmployeeSummary>) => ({...state, summary: action.payload}),
    setApproved: (state: State, action: PayloadAction<Vacation[]>) => ({...state, approved: action.payload}),
    setPending: (state: State, action: PayloadAction<Vacation[]>) => ({...state, pending: action.payload}),
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State, action: PayloadAction) => ({...state, addingRequest: true}),
    endAddRequest: (state: State, action: PayloadAction) => ({...state, currentRequest: '', addingRequest: false}),
  },
});

export const {
  setSummary,
  setApproved,
  setPending,
  setCurrentRequest,
  startAddRequest,
  endAddRequest,
} = slice.actions;

export const reducer = slice.reducer;

export const loadSummary = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const key = {employeeRole: {employee: ledger.party()}};
    const {data: {employeeRole: {employee, boss}, remainingDays}} =
      await ledger.pseudoFetchByKey(davl.EmployeeVacationAllocation, key);
    const summary: EmployeeSummary = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    }
    dispatch(setSummary(summary));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadApproved = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const vacationContracts =
      await ledger.query(davl.Vacation, {employeeRole: {employee: ledger.party()}});
    const vacations: Vacation[] =
      vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
    dispatch(setApproved(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadPending = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const requests =
      await ledger.query(davl.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
    const vacations: Vacation[] =
      requests.map((request) => makeVacation(request.contractId, request.data.vacation));
    dispatch(setPending(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadAll = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  await Promise.all([
    dispatch(loadSummary(ledger)),
    dispatch(loadApproved(ledger)),
    dispatch(loadPending(ledger)),
  ]);
}

export const addRequest = (ledger: Ledger, fromDate: string, toDate: string): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    dispatch(startAddRequest());
    const key = {employee: ledger.party()};
    await ledger.pseudoExerciseByKey(davl.EmployeeRole.RequestVacation, key, {fromDate, toDate});
    dispatch(endAddRequest());
    alert('Request successfully submitted.');
    await dispatch(loadPending(ledger));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
