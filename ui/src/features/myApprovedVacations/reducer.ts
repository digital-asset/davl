import { createSlice, PayloadAction } from 'redux-starter-kit';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import * as davl from '../../daml/DAVL';
import { Vacation, makeVacation } from '../../utils/vacation';

const initialState: Vacation[] = []

const slice = createSlice({
  name: 'myApprovedVacations',
  initialState,
  reducers: {
    set: (state, action: PayloadAction<Vacation[]>) => action.payload,
  },
});

export const { set } = slice.actions;

export const reducer = slice.reducer;

export const load = (ledger: Ledger): AppThunk<Promise<void>> => async (dispatch) => {
  try {
    const vacationContracts = await ledger.query(davl.Vacation, {employeeRole: {employee: ledger.party()}});
    const vacations: Vacation[] = vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.data));
    dispatch(set(vacations));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
