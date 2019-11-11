/* eslint-disable @typescript-eslint/class-name-casing */
/* eslint-disable @typescript-eslint/camelcase */
import { Party, Template, Choice, party, int, Int, Date, date, TemplateId } from '../../ledger/types';
import { object } from '@mojotech/json-type-validation';
import packageId from './packageId';

const moduleName = 'DAVL';
const templateId = (entityName: string): TemplateId => ({packageId, moduleName, entityName});

export type EmployeeRole_RequestVacation = {
  fromDate: Date;
  toDate: Date;
}

export const EmployeeRole_RequestVacation: Choice<EmployeeRole, EmployeeRole_RequestVacation> = {
  template: undefined as unknown as typeof EmployeeRole,
  choiceName: "EmployeeRole_RequestVacation",
  decoder: () => object({fromDate: date(), toDate: date()}),
}

export type EmployeeRole = {
  employee: Party;
  company: Party;
  boss: Party;
}

export const EmployeeRole: Template<EmployeeRole> & {
  RequestVacation: Choice<EmployeeRole, EmployeeRole_RequestVacation>;
} = {
  templateId: templateId("EmployeeRole"),
  decoder: () => object({
    employee: party(),
    company: party(),
    boss: party(),
  }),
  RequestVacation: EmployeeRole_RequestVacation,
}

EmployeeRole_RequestVacation.template = EmployeeRole;


export type EmployeeProposal_Accept = {}

export const EmployeeProposal_Accept: Choice<EmployeeProposal, EmployeeProposal_Accept> = {
  template: undefined as unknown as typeof EmployeeProposal,
  choiceName: "EmployeeProposal_Accept",
  decoder: () => object({}),
}

export type EmployeeProposal = {
  employeeRole: EmployeeRole;
  vacationDays: Int;
}

export const EmployeeProposal: Template<EmployeeProposal> & {
  Accept: Choice<EmployeeProposal, EmployeeProposal_Accept>;
} = {
  templateId: templateId("EmployeeProposal"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    vacationDays: int(),
  }),
  Accept: EmployeeProposal_Accept,
}

EmployeeProposal_Accept.template = EmployeeProposal;


export type EmployeeVacationAllocation = {
  employeeRole: EmployeeRole;
  remainingDays: Int;
}

export const EmployeeVacationAllocation: Template<EmployeeVacationAllocation> = {
  templateId: templateId("EmployeeVacationAllocation"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    remainingDays: int(),
  }),
}


export type Vacation = {
  employeeRole: EmployeeRole;
  fromDate: Date;
  toDate: Date;
}

export const Vacation: Template<Vacation> = {
  templateId: templateId("Vacation"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    fromDate: date(),
    toDate: date(),
  }),
}


export type VacationRequest_Accept = {}

export const VacationRequest_Accept: Choice<VacationRequest, VacationRequest_Accept> = {
  template: undefined as unknown as typeof VacationRequest,
  choiceName: "VacationRequest_Accept",
  decoder: () => object({}),
}

export type VacationRequest = {
  vacation: Vacation;
}

export const VacationRequest: Template<VacationRequest> & {
  Accept: Choice<VacationRequest, VacationRequest_Accept>;
} = {
  templateId: templateId("VacationRequest"),
  decoder: () => object({
    vacation: Vacation.decoder(),
  }),
  Accept: VacationRequest_Accept,
}

VacationRequest_Accept.template = VacationRequest;
