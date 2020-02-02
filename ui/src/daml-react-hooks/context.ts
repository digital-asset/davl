import Ledger from '@daml/ledger';
import * as LedgerStore from './ledgerStore';
import React from "react";
import { Action } from "./reducer";
import { Party } from '@daml/types';

export type DamlLedgerState = {
  store: LedgerStore.Store;
  dispatch: React.Dispatch<Action>;
  party: Party;
  ledger: Ledger;
}

export const DamlLedgerContext = React.createContext(null as DamlLedgerState | null);
