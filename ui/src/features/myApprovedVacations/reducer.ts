import { createSlice, PayloadAction } from 'redux-starter-kit';
import { Item as VacationListItem } from '../../components/VacationList';

let initialState: VacationListItem[] = []

const slice = createSlice({
  name: 'myApprovedVacations',
  initialState,
  reducers: {
    set: (state, action: PayloadAction<VacationListItem[]>) => action.payload,
  },
});

export const { set } = slice.actions;

export default slice.reducer;
