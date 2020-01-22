import React from 'react';
import Requests from './Requests';
import SummaryView from './SummaryView';
import { Segment } from 'semantic-ui-react';
import VacationListSegment from '../../components/VacationListSegment';
import { toast } from 'react-semantic-toasts';
import { useQuery, useParty, useFetchByKey } from '../../daml-react-hooks';
import * as v3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { splitVacations } from '../../utils/vacation';
import { EmployeeSummary } from '../../utils/employee';


const EmployeeView: React.FC = () => {
  const party = useParty();
  const allocation = useFetchByKey(v3.EmployeeVacationAllocation, () => party, [party]);
  let summary: EmployeeSummary | null = null;
  if (allocation.contract) {
    const {payload: {employeeRole: {employee, boss}, remainingDays}} = allocation.contract;
    summary = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    };
  }

  const {loading: loadingVacations, contracts: vacationContracts} =
    useQuery(v3.Vacation, () => ({employeeRole: {employee: party}}), [party]);
  const vacations = splitVacations(vacationContracts);

  const handleCancelVacation = () => toast({
    title: 'Not yet implemented',
    type: 'error',
    description: 'Canceling vacation is not yet implemented.',
    time: 0,
  });

  return (
    <Segment.Group>
      {summary ? <SummaryView {...summary} loading={allocation.loading} /> : <p>Loading summary...</p>}
      <Requests employeeSummary={summary} />
      <VacationListSegment
        header='Upcoming Vacations'
        loading={loadingVacations}
        viewer='employee'
        vacations={vacations.upcoming}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
      <VacationListSegment
        header='Past Vacations'
        loading={loadingVacations}
        viewer='employee'
        vacations={vacations.past}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
    </Segment.Group>
  );
}

export default EmployeeView;
