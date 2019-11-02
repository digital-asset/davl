import { createSlice, PayloadAction } from 'redux-starter-kit';
import { Item as VacationListItem, makeItem } from '../../components/VacationList';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { Vacation } from '../../daml/DAVL';

let initialState: VacationListItem[] = []

const slice = createSlice({
  name: 'myApprovedVacations',
  initialState,
  reducers: {
    set: (state, action: PayloadAction<VacationListItem[]>) => action.payload,
  },
});

export const { set } = slice.actions;

export const reducer = slice.reducer;

export const load = (ledger: Ledger): AppThunk => async (dispatch) => {
  try {
    const vacations = await ledger.query(Vacation, {employeeRole: {employee: ledger.party()}});
    const items: VacationListItem[] = vacations.map((vacation) => makeItem(vacation.contractId, vacation.data));
    dispatch(set(items));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
