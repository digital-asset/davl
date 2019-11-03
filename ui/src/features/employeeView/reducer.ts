import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import * as davl from '../../daml/DAVL';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';
import { EmployeeSummary } from './types';
import moment from 'moment';
import { partition } from 'fp-ts/lib/Array';
import { getDualOrd } from 'fp-ts/lib/Ord';

export type State = {
  summary: EmployeeSummary | null;
  pending: Vacation[];
  upcomingVacations: Vacation[];
  pastVacations: Vacation[];
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  summary: null,
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
  setCurrentRequest,
} = slice.actions;


export const reducer = slice.reducer;

const loadSummary = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
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

const loadVacations = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const vacationContracts =
      await ledger.query(davl.Vacation, {employeeRole: {employee: ledger.party()}});
    const vacations: Vacation[] =
      vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
    const today = moment().format('YYYY-MM-DD');
    const {left: upcomingVacations, right: pastVacations} =
      partition((vacation: Vacation) => vacation.fromDate <= today)(vacations);
    upcomingVacations.sort(ordVacationOnFromDate.compare);
    pastVacations.sort(getDualOrd(ordVacationOnFromDate).compare);
    dispatch(setUpcomingVacations(upcomingVacations));
    dispatch(setPastVacations(pastVacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

const loadRequests = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const requests =
      await ledger.query(davl.VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
    const vacations: Vacation[] =
      requests.map((request) => makeVacation(request.contractId, request.data.vacation));
    vacations.sort(ordVacationOnFromDate.compare);
    dispatch(setPending(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const loadAll = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  await Promise.all([
    dispatch(loadSummary(ledger)),
    dispatch(loadVacations(ledger)),
    dispatch(loadRequests(ledger)),
  ]);
}

export const addRequest = (ledger: Ledger, fromDate: string, toDate: string): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    dispatch(startAddRequest());
    const key = {employee: ledger.party()};
    await ledger.pseudoExerciseByKey(davl.EmployeeRole.RequestVacation, key, {fromDate, toDate});
    dispatch(endAddRequest());
    await dispatch(loadRequests(ledger));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
