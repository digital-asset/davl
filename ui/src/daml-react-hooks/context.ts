import Ledger from "./ledger";
import * as LedgerStore from './ledgerStore';
import React from "react";
import { Action } from "./reducer";

export type DamlLedgerState = {
  store: LedgerStore.Store;
  dispatch: React.Dispatch<Action>;
  ledger: Ledger;
}

export const DamlLedgerContext = React.createContext(null as DamlLedgerState | null);
