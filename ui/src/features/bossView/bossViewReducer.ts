import { AppThunk, getLedger } from '../../app/store';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { ContractId } from '@digitalasset/daml-json-types';
import { toast } from 'react-semantic-toasts';
import * as daml from '../../app/damlReducer';

export const approveRequest = (contractId: ContractId<v3.VacationRequest>): AppThunk => async (dispatch, getState) => {
  const ledger = getLedger(getState());
  await ledger.exercise(v3.VacationRequest.VacationRequest_Accept, contractId, {});
  toast({
    title: 'Success',
    type: 'success',
    time: 3000,
    description: 'Request successfully approved.',
  });
  await Promise.all([
    dispatch(daml.reloadTemplate(v3.EmployeeVacationAllocation)),
    dispatch(daml.reloadTemplate(v3.Vacation)),
    dispatch(daml.reloadTemplate(v3.VacationRequest)),
  ]);
}
