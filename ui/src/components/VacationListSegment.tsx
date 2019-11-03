import React from 'react';
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Props as VacationListProps } from './VacationList';

type Props = VacationListProps & {
  header: string;
};

const VacationListSegment: React.FC<Props> = (props) => (
  <Segment>
    <Header as='h3' content={props.header} />
    <VacationList {...props} />
  </Segment>
);

export default VacationListSegment;
