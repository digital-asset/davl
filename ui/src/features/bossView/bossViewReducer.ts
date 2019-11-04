import { createSlice, PayloadAction } from 'redux-starter-kit';
import { AppThunk, getLedger } from '../../app/store';
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
  await dispatch(loadRequests());
}

export const approveRequest = (contractId: ContractId<VacationRequest>): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState);
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  alert('Request successfully approved.');
  await dispatch(loadRequests());
}
