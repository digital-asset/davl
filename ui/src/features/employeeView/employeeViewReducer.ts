import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { EmployeeSummary } from '../../utils/employee';
import { toast } from 'react-semantic-toasts';
import { reload } from '../../app/damlReducer';

export type State = {
  summary: EmployeeSummary | undefined;
  currentRequest: string;
  loadingSummary: boolean;
  addingRequest: boolean;
}

const initialState: State = {
  summary: undefined,
  currentRequest: '',
  loadingSummary: false,
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    clearAll: () => initialState,
    setSummary: (state: State, action: PayloadAction<EmployeeSummary>) => ({...state, summary: action.payload}),
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State) => ({...state, addingRequest: true}),
    endAddRequest: (state: State) => ({...state, currentRequest: '', addingRequest: false}),
    setField: (state: State, action: PayloadAction<Partial<State>>) => ({...state, ...action.payload}),
  },
});

const {
  setSummary,
  startAddRequest,
  endAddRequest,
  setField,
} = slice.actions;

export const {
  clearAll,
  setCurrentRequest,
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

export const loadSummary = withLoading('loadingSummary', async (dispatch, getState) => {
  const ledger = getLedger(getState());
  const key = {employeeRole: {employee: ledger.party}};
  const {argument: {employeeRole: {employee, boss}, remainingDays}} =
    await ledger.pseudoFetchByKey(v3.EmployeeVacationAllocation, key);
  const summary: EmployeeSummary = {
    employee,
    boss,
    remainingVacationDays: remainingDays,
  }
  dispatch(setSummary(summary));
});

export const addRequest = (fromDate: string, toDate: string): AppThunk => async (dispatch, getState) => {
  try {
    dispatch(startAddRequest());
    const ledger = getLedger(getState());
    const key = {employee: ledger.party};
    await ledger.pseudoExerciseByKey(v3.EmployeeRole.EmployeeRole_RequestVacation, key, {fromDate, toDate});
  } finally {
    dispatch(endAddRequest());
  }
  toast({
    title:' Success',
    type: 'success',
    description: 'Request successfully submitted.',
    time: 3000,
  });
  await dispatch(reload(v3.VacationRequest));
}
