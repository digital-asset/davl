import { storiesOf } from "@storybook/react";
import React from "react";
import SummaryView from "./SummaryView";
import { EmployeeSummary } from "../../utils/employee";

const summary: EmployeeSummary = {
  employee: "Bob",
  boss: "Alice",
  remainingVacationDays: "25",
};

storiesOf("SummaryView", module).add("default", () => (
  <SummaryView {...summary} />
));
