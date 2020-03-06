import { storiesOf } from "@storybook/react";
import React from "react";
import VacationList, { Props } from "./VacationList";
import { Vacation } from "../utils/vacation";

const employeeRole = {
  employee: "Bob",
  boss: "Alice",
};

const vacations: Vacation[] = [
  {
    ...employeeRole,
    contractId: "1",
    fromDate: "2019-12-24",
    toDate: "2019-12-26",
    version: "v5",
  },
  {
    ...employeeRole,
    contractId: "2",
    fromDate: "2019-12-30",
    toDate: "2020-01-02",
    version: "v5",
  },
];

const props: Props = {
  vacations,
  viewer: "employee",
  onClickVacation: vacation => alert(JSON.stringify(vacation)),
  icon: "check",
};

storiesOf("VacationList", module).add("default", () => (
  <VacationList {...props} />
));
