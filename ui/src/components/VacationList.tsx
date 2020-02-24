import React from "react";
import { List } from "semantic-ui-react";
import { SemanticICONS } from "semantic-ui-react/dist/commonjs/generic";
import { Vacation } from "../utils/vacation";
import { VacationListItem } from "./VacationListItem";

export type Props = {
  vacations: Vacation[];
  viewer: "employee" | "boss";
  onClickVacation: (vacation: Vacation) => void;
  icon: SemanticICONS;
};

/**
 * React component to display a list of `Vacation`s.
 */
const VacationList: React.FC<Props> = ({
  vacations,
  viewer,
  onClickVacation,
  icon,
}) => {
  return (
    <List relaxed>
      {vacations.map(vacation => (
        <VacationListItem
          key={vacation.contractId}
          vacation={vacation}
          viewer={viewer}
          onClickIcon={onClickVacation}
          icon={icon}
        />
      ))}
    </List>
  );
};

export default VacationList;
