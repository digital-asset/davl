import React from 'react';
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Props as VacationListProps } from './VacationList';

type Props = VacationListProps & {
  header: string;
  loading?: boolean;
};

const VacationListSegment: React.FC<Props> = (props) => (
  <Segment loading={props.loading} >
    <Header as='h3' content={props.header} />
    <VacationList {...props} />
  </Segment>
);

export default VacationListSegment;
