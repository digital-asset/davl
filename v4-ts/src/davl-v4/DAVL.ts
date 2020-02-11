// Generated from DAVL.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

import * as pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template from './../d14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662/DA/Internal/Template';

export type Vacation = {
  employeeRole: EmployeeRole;
  fromDate: daml.Date;
  toDate: daml.Date;
}
export const Vacation: daml.Template<Vacation, undefined, '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:Vacation'> & {
  Archive: daml.Choice<Vacation, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, undefined>;
} = {
  templateId: '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:Vacation',
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    fromDate: daml.Date.decoder(),
    toDate: daml.Date.decoder(),
  }),
  Archive: {
    template: () => Vacation,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
daml.registerTemplate(Vacation);

export type VacationRequest_Accept = {
}
export const VacationRequest_Accept: daml.Serializable<VacationRequest_Accept> = ({
  decoder: () => jtv.object({
  }),
})

export type VacationRequest = {
  vacation: Vacation;
}
export const VacationRequest: daml.Template<VacationRequest, undefined, '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:VacationRequest'> & {
  VacationRequest_Accept: daml.Choice<VacationRequest, VacationRequest_Accept, daml.ContractId<Vacation>, undefined>;
  Archive: daml.Choice<VacationRequest, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, undefined>;
} = {
  templateId: '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:VacationRequest',
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    vacation: Vacation.decoder(),
  }),
  VacationRequest_Accept: {
    template: () => VacationRequest,
    choiceName: 'VacationRequest_Accept',
    argumentDecoder: VacationRequest_Accept.decoder,
    resultDecoder: () => daml.ContractId(Vacation).decoder(),
  },
  Archive: {
    template: () => VacationRequest,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
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
})

export type EmployeeVacationAllocation = {
  employeeRole: EmployeeRole;
  remainingDays: daml.Int;
}
export const EmployeeVacationAllocation: daml.Template<EmployeeVacationAllocation, EmployeeVacationAllocation.Key, '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeVacationAllocation'> & {
  EmployeeVacationAllocation_AddDays: daml.Choice<EmployeeVacationAllocation, EmployeeVacationAllocation_AddDays, {}, EmployeeVacationAllocation.Key>;
  Archive: daml.Choice<EmployeeVacationAllocation, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, EmployeeVacationAllocation.Key>;
} = {
  templateId: '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeVacationAllocation',
  keyDecoder: () => daml.Party.decoder(),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    remainingDays: daml.Int.decoder(),
  }),
  EmployeeVacationAllocation_AddDays: {
    template: () => EmployeeVacationAllocation,
    choiceName: 'EmployeeVacationAllocation_AddDays',
    argumentDecoder: EmployeeVacationAllocation_AddDays.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  Archive: {
    template: () => EmployeeVacationAllocation,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace EmployeeVacationAllocation {
  export type Key = daml.Party
}
daml.registerTemplate(EmployeeVacationAllocation);

export type EmployeeProposal_Accept = {
}
export const EmployeeProposal_Accept: daml.Serializable<EmployeeProposal_Accept> = ({
  decoder: () => jtv.object({
  }),
})

export type EmployeeProposal = {
  employeeRole: EmployeeRole;
  vacationDays: daml.Int;
}
export const EmployeeProposal: daml.Template<EmployeeProposal, undefined, '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeProposal'> & {
  Archive: daml.Choice<EmployeeProposal, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, undefined>;
  EmployeeProposal_Accept: daml.Choice<EmployeeProposal, EmployeeProposal_Accept, {}, undefined>;
} = {
  templateId: '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeProposal',
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employeeRole: EmployeeRole.decoder(),
    vacationDays: daml.Int.decoder(),
  }),
  Archive: {
    template: () => EmployeeProposal,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
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
})

export type EmployeeRole = {
  employee: daml.Party;
  company: daml.Party;
  boss: daml.Party;
}
export const EmployeeRole: daml.Template<EmployeeRole, EmployeeRole.Key, '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeRole'> & {
  Archive: daml.Choice<EmployeeRole, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, EmployeeRole.Key>;
  EmployeeRole_RequestVacation: daml.Choice<EmployeeRole, EmployeeRole_RequestVacation, daml.ContractId<VacationRequest>, EmployeeRole.Key>;
} = {
  templateId: '77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207:DAVL:EmployeeRole',
  keyDecoder: () => daml.Party.decoder(),
  decoder: () => jtv.object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
    boss: daml.Party.decoder(),
  }),
  Archive: {
    template: () => EmployeeRole,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  EmployeeRole_RequestVacation: {
    template: () => EmployeeRole,
    choiceName: 'EmployeeRole_RequestVacation',
    argumentDecoder: EmployeeRole_RequestVacation.decoder,
    resultDecoder: () => daml.ContractId(VacationRequest).decoder(),
  },
};
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace EmployeeRole {
  export type Key = daml.Party
}
daml.registerTemplate(EmployeeRole);
