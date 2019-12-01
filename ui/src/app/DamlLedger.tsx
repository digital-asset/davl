import React, { useReducer, useMemo } from 'react';
import { reducer, DamlLedgerContext } from './damlReducer';
import Credentials from '../ledger/credentials';
import * as LedgerStore from './ledgerStore';
import Ledger from '../ledger/ledger';

type Props = {
  credentials: Credentials;
}

export const DamlLedger: React.FC<Props> = (props) => {
  const dataDispatch = useReducer(reducer, LedgerStore.empty);
  const store = useMemo(() => ({
    data: dataDispatch[0],
    dispatch: dataDispatch[1],
    ledger: new Ledger(props.credentials),
  }), [props.credentials, dataDispatch])
  return (
    <DamlLedgerContext.Provider value={store}>
      {props.children}
    </DamlLedgerContext.Provider>
  );
}
