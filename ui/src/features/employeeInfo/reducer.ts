import { createSlice, PayloadAction } from 'redux-starter-kit';
import { Party } from '../../ledger/Types';
import Ledger from '../../ledger/Ledger';
import { AppThunk } from '../../app/store';
import { EmployeeVacationAllocation } from '../../daml/DAVL';

export type EmployeeInfo = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
}

const slice = createSlice({
  name: 'employeeInfo',
  initialState: null as EmployeeInfo | null,
  reducers: {
    set: (state, action: PayloadAction<EmployeeInfo>) => action.payload,
    reset: (state, action: PayloadAction<{}>) => undefined,
  },
});

export const { set, reset } = slice.actions;

export const reducer = slice.reducer;

export const load = (ledger: Ledger): AppThunk => async (dispatch) => {
  try {
    const key = {employeeRole: {employee: ledger.party()}};
    const {data: {employeeRole: {employee, boss}, remainingDays}} =
      await ledger.pseudoFetchByKey(EmployeeVacationAllocation, key);
    const employeeInfo: EmployeeInfo = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    }
    dispatch(set(employeeInfo));
  } catch (error) {
    alert(`Unknown error:\n${error}`);
  }
}
