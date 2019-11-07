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
  loadingStaff: boolean;
  loadingRequests: boolean;
  loadingVacations: boolean;
}

const initialState: State = {
  staff: [],
  requests: [],
  vacations: emptyVacations,
  loadingStaff: false,
  loadingRequests: false,
  loadingVacations: false,
}

const slice = createSlice({
  name: 'bossView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setStaff: (state: State, action: PayloadAction<EmployeeSummary[]>) => ({...state, staff: action.payload}),
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
    setVacations: (state: State, action: PayloadAction<Vacations>) => ({...state, vacations: action.payload}),
    setField: (state: State, action: PayloadAction<Partial<State>>) => ({...state, ...action.payload})
  },
});

const {
  setStaff,
  setRequests,
  setVacations,
  setField,
} = slice.actions;

export const {
  clearAll,
} = slice.actions;

export const reducer = slice.reducer;

const withLoading = <K extends {[L in keyof State]: State[L] extends boolean ? L : never}[keyof State]>
  (k: K, thunk: AppThunk) => (): AppThunk => async (dispatch, getState, extraArgument) => {
    try {
      dispatch(setField({[k]: true}));
      await thunk(dispatch, getState, extraArgument);
    } finally {
      dispatch(setField({[k]: false}));
    }
  }

const loadStaff = withLoading('loadingStaff', async (dispatch, getState) => {
  const ledger = getLedger(getState());
  const key = {employeeRole: {boss: ledger.party}};
  const allocations = await ledger.query(EmployeeVacationAllocation, key);
  const staff: EmployeeSummary[] = allocations.map((allocation) => ({
    employee: allocation.argument.employeeRole.employee,
    boss: allocation.argument.employeeRole.boss,
    remainingVacationDays: allocation.argument.remainingDays,
  }));
  staff.sort(ordEmployeeSummaryOnName.compare);
  dispatch(setStaff(staff));
});

const loadRequests = withLoading('loadingRequests', async (dispatch, getState) => {
  try {
    dispatch(setField({loadingRequests: true}));
    const ledger = getLedger(getState());
    const requestsContracts =
      await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party}}});
    const requests: Vacation[] =
      requestsContracts.map(({contractId, argument}) => makeVacation(contractId, argument.vacation));
    requests.sort(ordVacationOnFromDate.compare);
    dispatch(setRequests(requests));
  } finally {
    dispatch(setField({loadingRequests: false}));
  }
});

const loadVacations = withLoading('loadingVacations', async (dispatch, getState) => {
  try {
    dispatch(setField({loadingVacations: true}));
    const ledger = getLedger(getState());
    const vacationContracts =
      await ledger.query(DAVL.Vacation, {employeeRole: {boss: ledger.party}});
    const vacations: Vacation[] =
      vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.argument));
    dispatch(setVacations(splitVacations(vacations)));
  } finally {
    dispatch(setField({loadingVacations: false}));
  }
});

export const loadAll = (): AppThunk => async (dispatch) => {
  await Promise.all([
    dispatch(loadStaff()),
    dispatch(loadRequests()),
    dispatch(loadVacations()),
  ]);
}

export const approveRequest = (contractId: ContractId<VacationRequest>): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState());
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  toast({
    title: 'Success',
    type: 'success',
    time: 3000,
    description: 'Request successfully approved.',
  });
  await dispatch(loadAll());
}
