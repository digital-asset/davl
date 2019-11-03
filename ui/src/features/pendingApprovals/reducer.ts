import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { Vacation, makeVacation } from '../../utils/vacation';

const initialState: Vacation[] = []

const slice = createSlice({
  name: 'pendingApprovals',
  initialState,
  reducers: {
    set: (state, action: PayloadAction<Vacation[]>) => action.payload,
  },
});

export const { set } = slice.actions;

export const reducer = slice.reducer;

export const load = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const requests = await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
    const vacations: Vacation[] = requests.map((request) => makeVacation(request.contractId, request.data.vacation));
    dispatch(set(vacations));
} catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}

export const approveRequest = (ledger: Ledger, contractId: ContractId<VacationRequest>): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    await ledger.exercise(VacationRequest.Accept, contractId, {});
    alert('Request successfully approved.');
    await dispatch(load(ledger));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
