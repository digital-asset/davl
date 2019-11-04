import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';

type State = {
  requests: Vacation[];
}

const initialState: State = {
  requests: [],
}

const slice = createSlice({
  name: 'bossView',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
  },
});

const { setRequests } = slice.actions;

export const {
  clearAll,
} = slice.actions;

export const reducer = slice.reducer;

const loadRequests = (ledger: Ledger): AppThunk => async (dispatch) => {
  const requestsContracts =
    await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
  const reqeuests: Vacation[] =
    requestsContracts.map(({contractId, data}) => makeVacation(contractId, data.vacation));
  reqeuests.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(reqeuests));
}

export const loadAll = (ledger: Ledger): AppThunk => async (dispatch) => {
  await dispatch(loadRequests(ledger));
}

export const approveRequest = (ledger: Ledger, contractId: ContractId<VacationRequest>): AppThunk => async (dispatch) => {
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  alert('Request successfully approved.');
  await dispatch(loadRequests(ledger));
}
