import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { toast } from 'react-semantic-toasts';
import { reloadTemplate } from '../../app/damlReducer';

export type State = {
  currentRequest: string;
  addingRequest: boolean;
}

const initialState: State = {
  currentRequest: '',
  addingRequest: false,
}

const slice = createSlice({
  name: 'employeeView',
  initialState,
  reducers: {
    clearAll: () => initialState,
    setCurrentRequest: (state: State, action: PayloadAction<string>) => ({...state, currentRequest: action.payload}),
    startAddRequest: (state: State) => ({...state, addingRequest: true}),
    endAddRequest: (state: State) => ({...state, currentRequest: '', addingRequest: false}),
  },
});

const {
  startAddRequest,
  endAddRequest,
} = slice.actions;

export const {
  clearAll,
  setCurrentRequest,
} = slice.actions;


export const reducer = slice.reducer;

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
  await dispatch(reloadTemplate(v3.VacationRequest));
}
