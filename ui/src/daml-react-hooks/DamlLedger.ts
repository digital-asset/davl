// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import React, { useMemo, useState } from 'react';
import { DamlLedgerContext, DamlLedgerState } from './context';
import Ledger from './streamLedger';
import { Party } from '@daml/types';

type Props = {
  token: string;
  httpBaseUrl?: string;
  wsBaseUrl?: string;
  party: Party;
}

const DamlLedger: React.FC<Props> = ({token, httpBaseUrl, wsBaseUrl, party, children}) => {
  const [reloadToken, setReloadToken] = useState(0);
  const ledger = useMemo(() => new Ledger({token, httpBaseUrl, wsBaseUrl}), [token, httpBaseUrl, wsBaseUrl]);
  const state: DamlLedgerState = useMemo(() => ({
    reloadToken,
    triggerReload: () => setReloadToken(x => x +1),
    party,
    ledger,
  }), [party, ledger, reloadToken]);
  return React.createElement(DamlLedgerContext.Provider, {value: state}, children);
}

export default DamlLedger;
