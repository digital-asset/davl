import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { ContractId } from '@digitalasset/daml-json-types';
import { Vacation, makeVacation, Vacations, emptyVacations, splitVacations } from '../../utils/vacation';
import { toast } from 'react-semantic-toasts';
import { EmployeeSummary, ordEmployeeSummaryOnName } from '../../utils/employee';
import * as daml from '../../app/damlReducer';

type State = {
  staff: EmployeeSummary[];
  vacations: Vacations;
  loadingStaff: boolean;
  loadingVacations: boolean;
}

const initialState: State = {
  staff: [],
  vacations: emptyVacations,
  loadingStaff: false,
  loadingVacations: false,
}

const slice = createSlice({
  name: 'bossView',
  initialState,
  reducers: {
    clearAll: () => initialState,
    setStaff: (state: State, action: PayloadAction<EmployeeSummary[]>) => ({...state, staff: action.payload}),
    setVacations: (state: State, action: PayloadAction<Vacations>) => ({...state, vacations: action.payload}),
    setField: (state: State, action: PayloadAction<Partial<State>>) => ({...state, ...action.payload})
  },
});

const {
  setStaff,
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
  const allocations = await ledger.query(v3.EmployeeVacationAllocation, key);
  const staff: EmployeeSummary[] = allocations.map((allocation) => ({
    employee: allocation.argument.employeeRole.employee,
    boss: allocation.argument.employeeRole.boss,
    remainingVacationDays: allocation.argument.remainingDays,
  }));
  staff.sort(ordEmployeeSummaryOnName.compare);
  dispatch(setStaff(staff));
});

const loadVacations = withLoading('loadingVacations', async (dispatch, getState) => {
  try {
    dispatch(setField({loadingVacations: true}));
    const ledger = getLedger(getState());
    const vacationContracts =
      await ledger.query(v3.Vacation, {employeeRole: {boss: ledger.party}});
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
    dispatch(loadVacations()),
  ]);
}

export const approveRequest = (contractId: ContractId<v3.VacationRequest>): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState());
  await ledger.exercise(v3.VacationRequest.VacationRequest_Accept, contractId, {});
  toast({
    title: 'Success',
    type: 'success',
    time: 3000,
    description: 'Request successfully approved.',
  });
  await Promise.all([
    dispatch(loadAll()),
    dispatch(daml.reload(v3.VacationRequest))
  ]);
}
