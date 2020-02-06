// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import React, { useMemo, useState } from 'react';
import { DamlLedgerContext, DamlLedgerState } from './context';
import Ledger from './streamLedger';
import { Party } from '@daml/types';

type Props = {
  token: string;
  party: Party;
}

const DamlLedger: React.FC<Props> = (props) => {
  const [reloadToken, setReloadToken] = useState(0);
  const ledger = useMemo(() => new Ledger(props.token), [props.token]);
  const state: DamlLedgerState = useMemo(() => ({
    reloadToken,
    triggerReload: () => setReloadToken(x => x +1),
    party: props.party,
    ledger,
  }), [props.party, ledger, reloadToken]);
  return React.createElement(DamlLedgerContext.Provider, {value: state}, props.children);
}

export default DamlLedger;
