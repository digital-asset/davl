// Generated from DAVL.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

import * as pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template from './../cc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da/DA/Internal/Template';

import packageId from './packageId';
const moduleName = 'DAVL';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Vacation = {
  employeeRole: EmployeeRole;
  fromDate: daml.Date;
  toDate: daml.Date;
}
export const Vacation: daml.Template<Vacation, undefined> & {
  Archive: daml.Choice<Vacation, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive, {} >;
} = {
  templateId: templateId('Vacation'),
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
  Archive: {
    template: () => Vacation,
    choiceName: 'Archive',
    argumentDecoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
daml.registerTemplate(Vacation);

export type VacationRequest_Accept = {
}
export const VacationRequest_Accept: daml.Serializable<VacationRequest_Accept> = ({
  decoder: () => jtv.object({
  }),
});

export type VacationRequest = {
  vacation: Vacation;
}
export const VacationRequest: daml.Template<VacationRequest, undefined> & {
  Archive: daml.Choice<VacationRequest, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive, {} >;
  VacationRequest_Accept: daml.Choice<VacationRequest, VacationRequest_Accept, daml.ContractId<Vacation> >;
} = {
  templateId: templateId('VacationRequest'),
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    vacation: Vacation.decoder(),
  }),
  Archive: {
    template: () => VacationRequest,
    choiceName: 'Archive',
    argumentDecoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  VacationRequest_Accept: {
    template: () => VacationRequest,
    choiceName: 'VacationRequest_Accept',
    argumentDecoder: VacationRequest_Accept.decoder,
    resultDecoder: () => daml.ContractId(Vacation).decoder(),
  },
};
daml.registerTemplate(VacationRequest);

export type EmployeeVacationAllocation_AddDays = {
  days: daml.Int;
}
export const EmployeeVacationAllocation_AddDays: daml.Serializable<EmployeeVacationAllocation_AddDays> = ({
  decoder: () => jtv.object({
    days: daml.Int.decoder(),
  }),
});

export type EmployeeVacationAllocation = {
  employeeRole: EmployeeRole;
  remainingDays: daml.Int;
}
export const EmployeeVacationAllocation: daml.Template<EmployeeVacationAllocation, daml.Party> & {
  Archive: daml.Choice<EmployeeVacationAllocation, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive, {} >;
  EmployeeVacationAllocation_AddDays: daml.Choice<EmployeeVacationAllocation, EmployeeVacationAllocation_AddDays, {} >;
} = {
  templateId: templateId('EmployeeVacationAllocation'),
  keyDecoder: () => daml.Party.decoder(),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    remainingDays: daml.Int.decoder(),
  }),
  Archive: {
    template: () => EmployeeVacationAllocation,
    choiceName: 'Archive',
    argumentDecoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  EmployeeVacationAllocation_AddDays: {
    template: () => EmployeeVacationAllocation,
    choiceName: 'EmployeeVacationAllocation_AddDays',
    argumentDecoder: EmployeeVacationAllocation_AddDays.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
daml.registerTemplate(EmployeeVacationAllocation);

export type EmployeeProposal_Accept = {
}
export const EmployeeProposal_Accept: daml.Serializable<EmployeeProposal_Accept> = ({
  decoder: () => jtv.object({
  }),
});

export type EmployeeProposal = {
  employeeRole: EmployeeRole;
  vacationDays: daml.Int;
}
export const EmployeeProposal: daml.Template<EmployeeProposal, undefined> & {
  Archive: daml.Choice<EmployeeProposal, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive, {} >;
  EmployeeProposal_Accept: daml.Choice<EmployeeProposal, EmployeeProposal_Accept, {} >;
} = {
  templateId: templateId('EmployeeProposal'),
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    vacationDays: daml.Int.decoder(),
  }),
  Archive: {
    template: () => EmployeeProposal,
    choiceName: 'Archive',
    argumentDecoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  EmployeeProposal_Accept: {
    template: () => EmployeeProposal,
    choiceName: 'EmployeeProposal_Accept',
    argumentDecoder: EmployeeProposal_Accept.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
daml.registerTemplate(EmployeeProposal);

export type EmployeeRole_RequestVacation = {
  fromDate: daml.Date;
  toDate: daml.Date;
}
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
}
export const EmployeeRole: daml.Template<EmployeeRole, daml.Party> & {
  Archive: daml.Choice<EmployeeRole, pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive, {} >;
  EmployeeRole_RequestVacation: daml.Choice<EmployeeRole, EmployeeRole_RequestVacation, daml.ContractId<VacationRequest> >;
} = {
  templateId: templateId('EmployeeRole'),
  keyDecoder: () => daml.Party.decoder(),
  decoder: () => jtv.object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
    boss: daml.Party.decoder(),
  }),
  Archive: {
    template: () => EmployeeRole,
    choiceName: 'Archive',
    argumentDecoder: pkgcc6d52aa624250119006cd19d51c60006762bd93ca5a6d288320a703024b33da_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  EmployeeRole_RequestVacation: {
    template: () => EmployeeRole,
    choiceName: 'EmployeeRole_RequestVacation',
    argumentDecoder: EmployeeRole_RequestVacation.decoder,
    resultDecoder: () => daml.ContractId(VacationRequest).decoder(),
  },
};
daml.registerTemplate(EmployeeRole);
