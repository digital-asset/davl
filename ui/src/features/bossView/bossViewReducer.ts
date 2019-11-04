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
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
  },
});

const { setRequests } = slice.actions;

export const reducer = slice.reducer;

export const loadRequests = (ledger: Ledger): AppThunk => async (dispatch) => {
  const requestsContracts =
    await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
  const reqeuests: Vacation[] =
    requestsContracts.map(({contractId, data}) => makeVacation(contractId, data.vacation));
  reqeuests.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(reqeuests));
}

export const approveRequest = (ledger: Ledger, contractId: ContractId<VacationRequest>): AppThunk => async (dispatch) => {
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  alert('Request successfully approved.');
  await dispatch(loadRequests(ledger));
}
