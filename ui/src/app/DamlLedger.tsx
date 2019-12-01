import React, { useReducer } from 'react';
import { reducer, DamlLedgerContext } from './damlReducer';
import Credentials from '../ledger/credentials';
import * as LedgerStore from './ledgerStore';

type Props = {
  credentials: Credentials;
}

export const DamlLedger: React.FC<Props> = (props) => {
  const reducerHandle = useReducer(reducer, LedgerStore.empty(props.credentials));
  return (
    <DamlLedgerContext.Provider value={reducerHandle}>
      {props.children}
    </DamlLedgerContext.Provider>
  );
}
