import { createSlice, PayloadAction, Action } from 'redux-starter-kit'
import Credentials from '../ledger/credentials'
import { AppThunk } from './store';
import Ledger from '../ledger/ledger';
import * as DAVL from '../daml/DAVL';
import { ThunkAction } from 'redux-thunk';
import { RootState } from './rootReducer';
import * as bossView from '../features/bossView/bossViewReducer';
import * as employeeView from '../features/employeeView/employeeViewReducer';

type State = {
  credentials?: Credentials;
  loggingIn: boolean;
  signingUp: boolean;
}

const initialState: State = {
  loggingIn: false,
  signingUp: false,
}

const slice = createSlice({
  name: 'auth',
  initialState,
  reducers: {
    clearAll: (state: State, action: PayloadAction) => initialState,
    setCredentials: (state: State, action: PayloadAction<Credentials>) => ({...state, credentials: action.payload}),
    startLogIn: (state: State, action: PayloadAction) => ({...state, loggingIn: true}),
    endLogIn: (state: State, action: PayloadAction) => ({...state, loggingIn: false}),
    startSignUp: (state: State, action: PayloadAction) => ({...state, signingUp: true}),
    endSignUp: (state: State, action: PayloadAction) => ({...state, signingUp: false}),
  },
});

const {
  setCredentials,
  clearAll,
  startLogIn,
  endLogIn,
  startSignUp,
  endSignUp,
} = slice.actions;

export const reducer = slice.reducer;

export const logIn = (credentials: Credentials): AppThunk => async (dispatch) => {
  try {
    dispatch(startLogIn());
    const ledger = new Ledger(credentials);
    const employeeRole = await ledger.pseudoLookupByKey(DAVL.EmployeeRole, {employee: credentials.party});
    if (employeeRole) {
      dispatch(setCredentials(credentials));
    } else {
      alert("You have not yet signed up.");
    }
  } finally {
    dispatch(endLogIn());
  }
}

export const logOut = (): ThunkAction<void, RootState, null, Action<string>> => (dispatch) => {
  dispatch(clearAll());
  dispatch(employeeView.clearAll());
  dispatch(bossView.clearAll());
}

export const signUp = (credentials: Credentials): AppThunk => async (dispatch) => {
  try {
    dispatch(startSignUp());
    const ledger = new Ledger(credentials);
    const employeeProposals =
      await ledger.query(DAVL.EmployeeProposal, {employeeRole: {employee: credentials.party}});
    if (employeeProposals.length === 0) {
      alert("There is not invitation for you.");
    } else if(employeeProposals.length > 1) {
      alert("There are multiple invitations for you.");
    } else {
      const employeeProposalFull = employeeProposals[0];
      const employeeProposal = employeeProposalFull.argument;
      const employeeRole = employeeProposal.employeeRole;
      const accept = window.confirm(`You have been invited to work for ${employeeRole.company}.\nBoss: ${employeeRole.boss}\nVacation days: ${employeeProposal.vacationDays}\nDo you accept?`);
      if (accept) {
        await ledger.exercise(DAVL.EmployeeProposal.Accept, employeeProposalFull.contractId, {});
        dispatch(endSignUp());
        await dispatch(logIn(credentials));
      } else {
        dispatch(endSignUp());
      }
    }
  } catch (error) {
    dispatch(endSignUp());
    throw error;
  }
}
