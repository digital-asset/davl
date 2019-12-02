// Generated from DAVL.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';

import * as pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template from './../cc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da/DA/Internal/Template';

import packageId from './packageId';
const moduleName = 'DAVL';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Vacation = {
  employeeRole: EmployeeRole;
  fromDate: daml.Date;
  toDate: daml.Date;
};
export const Vacation: daml.Template<Vacation> & {
  Archive: daml.Choice<Vacation, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive>;
} = {
  templateId: templateId('Vacation'),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
  Archive: {
    template: undefined as unknown as daml.Template<Vacation>,
    choiceName: 'Archive',
    decoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
  },
};
Vacation.Archive.template = Vacation;
daml.registerTemplate(Vacation);

export type VacationRequest_Accept = {
};
export const VacationRequest_Accept: daml.Serializable<VacationRequest_Accept> = ({
  decoder: () => jtv.object({
  }),
});

export type VacationRequest = {
  vacation: Vacation;
};
export const VacationRequest: daml.Template<VacationRequest> & {
  Archive: daml.Choice<VacationRequest, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive>;
  VacationRequest_Accept: daml.Choice<VacationRequest, VacationRequest_Accept>;
} = {
  templateId: templateId('VacationRequest'),
  decoder: () => jtv.object({
    vacation: Vacation.decoder(),
  }),
  Archive: {
    template: undefined as unknown as daml.Template<VacationRequest>,
    choiceName: 'Archive',
    decoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
  },
  VacationRequest_Accept: {
    template: undefined as unknown as daml.Template<VacationRequest>,
    choiceName: 'VacationRequest_Accept',
    decoder: VacationRequest_Accept.decoder,
  },
};
VacationRequest.Archive.template = VacationRequest;
VacationRequest.VacationRequest_Accept.template = VacationRequest;
daml.registerTemplate(VacationRequest);

export type EmployeeVacationAllocation_AddDays = {
  days: daml.Int;
};
export const EmployeeVacationAllocation_AddDays: daml.Serializable<EmployeeVacationAllocation_AddDays> = ({
  decoder: () => jtv.object({
    days: daml.Int.decoder(),
  }),
});

export type EmployeeVacationAllocation = {
  employeeRole: EmployeeRole;
  remainingDays: daml.Int;
};
export const EmployeeVacationAllocation: daml.Template<EmployeeVacationAllocation> & {
  Archive: daml.Choice<EmployeeVacationAllocation, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive>;
  EmployeeVacationAllocation_AddDays: daml.Choice<EmployeeVacationAllocation, EmployeeVacationAllocation_AddDays>;
} = {
  templateId: templateId('EmployeeVacationAllocation'),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    remainingDays: daml.Int.decoder(),
  }),
  Archive: {
    template: undefined as unknown as daml.Template<EmployeeVacationAllocation>,
    choiceName: 'Archive',
    decoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
  },
  EmployeeVacationAllocation_AddDays: {
    template: undefined as unknown as daml.Template<EmployeeVacationAllocation>,
    choiceName: 'EmployeeVacationAllocation_AddDays',
    decoder: EmployeeVacationAllocation_AddDays.decoder,
  },
};
EmployeeVacationAllocation.Archive.template = EmployeeVacationAllocation;
EmployeeVacationAllocation.EmployeeVacationAllocation_AddDays.template = EmployeeVacationAllocation;
daml.registerTemplate(EmployeeVacationAllocation);

export type EmployeeProposal_Accept = {
};
export const EmployeeProposal_Accept: daml.Serializable<EmployeeProposal_Accept> = ({
  decoder: () => jtv.object({
  }),
});

export type EmployeeProposal = {
  employeeRole: EmployeeRole;
  vacationDays: daml.Int;
};
export const EmployeeProposal: daml.Template<EmployeeProposal> & {
  Archive: daml.Choice<EmployeeProposal, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive>;
  EmployeeProposal_Accept: daml.Choice<EmployeeProposal, EmployeeProposal_Accept>;
} = {
  templateId: templateId('EmployeeProposal'),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    vacationDays: daml.Int.decoder(),
  }),
  Archive: {
    template: undefined as unknown as daml.Template<EmployeeProposal>,
    choiceName: 'Archive',
    decoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
  },
  EmployeeProposal_Accept: {
    template: undefined as unknown as daml.Template<EmployeeProposal>,
    choiceName: 'EmployeeProposal_Accept',
    decoder: EmployeeProposal_Accept.decoder,
  },
};
EmployeeProposal.Archive.template = EmployeeProposal;
EmployeeProposal.EmployeeProposal_Accept.template = EmployeeProposal;
daml.registerTemplate(EmployeeProposal);

export type EmployeeRole_RequestVacation = {
  fromDate: daml.Date;
  toDate: daml.Date;
};
export const EmployeeRole_RequestVacation: daml.Serializable<EmployeeRole_RequestVacation> = ({
  decoder: () => jtv.object({
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
});

export type EmployeeRole = {
  employee: daml.Party;
  company: daml.Party;
  boss: daml.Party;
};
export const EmployeeRole: daml.Template<EmployeeRole> & {
  Archive: daml.Choice<EmployeeRole, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive>;
  EmployeeRole_RequestVacation: daml.Choice<EmployeeRole, EmployeeRole_RequestVacation>;
} = {
  templateId: templateId('EmployeeRole'),
  decoder: () => jtv.object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
    boss: daml.Party.decoder(),
  }),
  Archive: {
    template: undefined as unknown as daml.Template<EmployeeRole>,
    choiceName: 'Archive',
    decoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
  },
  EmployeeRole_RequestVacation: {
    template: undefined as unknown as daml.Template<EmployeeRole>,
    choiceName: 'EmployeeRole_RequestVacation',
    decoder: EmployeeRole_RequestVacation.decoder,
  },
};
EmployeeRole.Archive.template = EmployeeRole;
EmployeeRole.EmployeeRole_RequestVacation.template = EmployeeRole;
daml.registerTemplate(EmployeeRole);
