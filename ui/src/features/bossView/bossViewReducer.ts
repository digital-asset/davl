import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import { EmployeeVacationAllocation, VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';
import { toast } from 'react-semantic-toasts';
import { EmployeeSummary } from '../../utils/employee';

type State = {
  staff: EmployeeSummary[];
  requests: Vacation[];
}

const initialState: State = {
  staff: [],
  requests: [],
}

const slice = createSlice({
  name: 'bossView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setStaff: (state: State, action: PayloadAction<EmployeeSummary[]>) => ({...state, staff: action.payload}),
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
  },
});

const {
  setStaff,
  setRequests,
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
    employee: allocation.data.employeeRole.employee,
    boss: allocation.data.employeeRole.boss,
    remainingVacationDays: allocation.data.remainingDays,
  }));
  dispatch(setStaff(staff));
}

const loadRequests = (): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  const requestsContracts =
    await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
  const requests: Vacation[] =
    requestsContracts.map(({contractId, data}) => makeVacation(contractId, data.vacation));
  requests.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(requests));
}

export const loadAll = (): AppThunk => async (dispatch) => {
  await Promise.all([
    dispatch(loadStaff()),
    dispatch(loadRequests()),
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
  await dispatch(loadRequests());
}
