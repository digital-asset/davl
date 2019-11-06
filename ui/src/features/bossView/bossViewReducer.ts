import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import { EmployeeVacationAllocation, VacationRequest } from '../../daml/DAVL';
import * as DAVL from '../../daml/DAVL';
import { ContractId } from '../../ledger/types';
import { Vacation, makeVacation, ordVacationOnFromDate, Vacations, emptyVacations, splitVacations } from '../../utils/vacation';
import { toast } from 'react-semantic-toasts';
import { EmployeeSummary, ordEmployeeSummaryOnName } from '../../utils/employee';

type State = {
  staff: EmployeeSummary[];
  requests: Vacation[];
  vacations: Vacations;
}

const initialState: State = {
  staff: [],
  requests: [],
  vacations: emptyVacations,
}

const slice = createSlice({
  name: 'bossView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setStaff: (state: State, action: PayloadAction<EmployeeSummary[]>) => ({...state, staff: action.payload}),
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
    setVacations: (state: State, action: PayloadAction<Vacations>) => ({...state, vacations: action.payload}),
  },
});

const {
  setStaff,
  setRequests,
  setVacations,
} = slice.actions;

export const {
  clearAll,
} = slice.actions;

export const reducer = slice.reducer;

const loadStaff = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const key = {employeeRole: {boss: ledger.party()}};
  const allocations = await ledger.query(EmployeeVacationAllocation, key);
  const staff: EmployeeSummary[] = allocations.map((allocation) => ({
    employee: allocation.argument.employeeRole.employee,
    boss: allocation.argument.employeeRole.boss,
    remainingVacationDays: allocation.argument.remainingDays,
  }));
  staff.sort(ordEmployeeSummaryOnName.compare);
  dispatch(setStaff(staff));
}

const loadRequests = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const requestsContracts =
    await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
  const requests: Vacation[] =
    requestsContracts.map(({contractId, argument}) => makeVacation(contractId, argument.vacation));
  requests.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(requests));
}

const loadVacations = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const vacationContracts =
    await ledger.query(DAVL.Vacation, {employeeRole: {boss: ledger.party()}});
  const vacations: Vacation[] =
    vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.argument));
  dispatch(setVacations(splitVacations(vacations)));
}

export const loadAll = (): AppThunk => async (dispatch) => {
  await Promise.all([
    dispatch(loadStaff()),
    dispatch(loadRequests()),
    dispatch(loadVacations()),
  ]);
}

export const approveRequest = (contractId: ContractId<VacationRequest>): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  toast({
    title: 'Success',
    type: 'success',
    time: 3000,
    description: 'Request successfully approved.',
  });
  await dispatch(loadAll());
}
