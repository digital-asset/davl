import React from 'react';
import Ledger from '../../ledger/Ledger';
import Pending from './Pending';

type Props = {
  ledger: Ledger;
}

const BossView: React.FC<Props> = ({ledger}) => <Pending ledger={ledger} />

export default BossView;
