/* eslint-disable @typescript-eslint/camelcase */
import * as daml from '../../ledger/types';
import { object } from '@mojotech/json-type-validation';
import packageId from './packageId';

const moduleName = 'DAVL';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type EmployeeRole_RequestVacation = {
  fromDate: daml.Date;
  toDate: daml.Date;
}

export const EmployeeRole_RequestVacation: daml.Serializable<EmployeeRole_RequestVacation> = {
  decoder: () => object({
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
}

export type EmployeeRole = {
  employee: daml.Party;
  company: daml.Party;
  boss: daml.Party;
}

export const EmployeeRole: daml.Template<EmployeeRole> & {
  RequestVacation: daml.Choice<EmployeeRole, EmployeeRole_RequestVacation>;
} = {
  templateId: templateId("EmployeeRole"),
  decoder: () => object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
    boss: daml.Party.decoder(),
  }),
  RequestVacation: {
    template: undefined as unknown as daml.Template<EmployeeRole>,
    choiceName: "EmployeeRole_RequestVacation",
    decoder: EmployeeRole_RequestVacation.decoder,
  }
}

EmployeeRole.RequestVacation.template = EmployeeRole;


export type EmployeeProposal_Accept = {}

export const EmployeeProposal_Accept: daml.Serializable<EmployeeProposal_Accept> = {
  decoder: () => object({}),
}

export type EmployeeProposal = {
  employeeRole: EmployeeRole;
  vacationDays: daml.Int;
}

export const EmployeeProposal: daml.Template<EmployeeProposal> & {
  Accept: daml.Choice<EmployeeProposal, EmployeeProposal_Accept>;
} = {
  templateId: templateId("EmployeeProposal"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    vacationDays: daml.Int.decoder(),
  }),
  Accept: {
    template: undefined as unknown as daml.Template<EmployeeProposal>,
    choiceName: "EmployeeProposal_Accept",
    decoder: EmployeeProposal_Accept.decoder,
  }
}

EmployeeProposal.Accept.template = EmployeeProposal;


export type EmployeeVacationAllocation = {
  employeeRole: EmployeeRole;
  remainingDays: daml.Int;
}

export const EmployeeVacationAllocation: daml.Template<EmployeeVacationAllocation> = {
  templateId: templateId("EmployeeVacationAllocation"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    remainingDays: daml.Int.decoder(),
  }),
}


export type Vacation = {
  employeeRole: EmployeeRole;
  fromDate: daml.Date;
  toDate: daml.Date;
}

export const Vacation: daml.Template<Vacation> & {
} = {
  templateId: templateId("Vacation"),
  decoder: () => object({
    employeeRole: EmployeeRole.decoder(),
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
}


export type VacationRequest_Accept = {}

export const VacationRequest_Accept: daml.Serializable<VacationRequest_Accept> = {
  decoder: () => object({}),
}

export type VacationRequest = {
  vacation: Vacation;
}

export const VacationRequest: daml.Template<VacationRequest> & {
  Accept: daml.Choice<VacationRequest, VacationRequest_Accept>;
} = {
  templateId: templateId("VacationRequest"),
  decoder: () => object({
    vacation: Vacation.decoder(),
  }),
  Accept: {
    template: undefined as unknown as daml.Template<VacationRequest>,
    choiceName: "VacationRequest_Accept",
    decoder: VacationRequest_Accept.decoder,
  }
}

VacationRequest.Accept.template = VacationRequest;
