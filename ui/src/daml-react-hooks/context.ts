import Ledger from '@digitalasset/daml-ledger-fetch';
import * as LedgerStore from './ledgerStore';
import React from "react";
import { Action } from "./reducer";
import { Party } from '@digitalasset/daml-json-types';

export type DamlLedgerState = {
  store: LedgerStore.Store;
  dispatch: React.Dispatch<Action>;
  party: Party;
  ledger: Ledger;
}

export const DamlLedgerContext = React.createContext(null as DamlLedgerState | null);
