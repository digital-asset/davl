// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import React, { useReducer, useMemo } from 'react';
import { DamlLedgerContext } from './context';
import * as LedgerStore from './ledgerStore';
import Ledger from '@daml/ledger';
import { Party } from '@daml/types';
import { reducer } from './reducer';

type Props = {
  token: string;
  party: Party;
}

const DamlLedger: React.FC<Props> = (props) => {
  const [store, dispatch] = useReducer(reducer, LedgerStore.empty());
  const ledger = useMemo(() => new Ledger(props.token), [props.token]);
  const state = useMemo(() => ({
    store,
    dispatch,
    party: props.party,
    ledger,
  }), [props.party, ledger, store, dispatch]);
  return React.createElement(DamlLedgerContext.Provider, {value: state}, props.children);
}

export default DamlLedger;
