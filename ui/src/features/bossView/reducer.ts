import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { Vacation, makeVacation, ordVacationOnFromDate } from '../../utils/vacation';
import {} from 'fp-ts/lib/Array';

type State = {
  requests: Vacation[];
}

const initialState: State = {
  requests: [],
}

const slice = createSlice({
  name: 'pendingApprovals',
  initialState,
  reducers: {
    setRequests: (state: State, action: PayloadAction<Vacation[]>) => ({...state, requests: action.payload}),
  },
});

const { setRequests } = slice.actions;

export const reducer = slice.reducer;

export const loadRequests = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  const requests = await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
  const vacations: Vacation[] = requests.map((request) => makeVacation(request.contractId, request.data.vacation));
  vacations.sort(ordVacationOnFromDate.compare);
  dispatch(setRequests(vacations));
}

export const approveRequest = (ledger: Ledger, contractId: ContractId<VacationRequest>): AppThunk<Promise<void>> => async (dispatch) => {
  await ledger.exercise(VacationRequest.Accept, contractId, {});
  alert('Request successfully approved.');
  await dispatch(loadRequests(ledger));
}
