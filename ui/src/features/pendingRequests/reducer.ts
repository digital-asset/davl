import { createSlice, PayloadAction } from 'redux-starter-kit';
import { Item as VacationListItem, makeItem } from '../../components/VacationList';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { VacationRequest } from '../../daml/DAVL';

const initialState: VacationListItem[] = []

const slice = createSlice({
  name: 'pendingRequests',
  initialState,
  reducers: {
    set: (state, action: PayloadAction<VacationListItem[]>) => action.payload,
  },
});

export const { set } = slice.actions;

export const reducer = slice.reducer;

export const load = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const requests = await ledger.query(VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
    const items: VacationListItem[] = requests.map((request) => makeItem(request.contractId, request.data.vacation));
    dispatch(set(items));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
