import React from 'react';
import { Segment, Header, SemanticICONS } from 'semantic-ui-react';
import VacationList from './VacationList';
import { Vacation } from '../utils/vacation';

type Props = {
  header: string;
  vacations: Vacation[];
  onClickVacation: (vacation: Vacation) => void;
  icon: SemanticICONS;
}

const VacationListSegment: React.FC<Props> = (props) => (
  <Segment>
    <Header as='h3' content={props.header} />
    <VacationList {...props} />
  </Segment>
);

export default VacationListSegment;
