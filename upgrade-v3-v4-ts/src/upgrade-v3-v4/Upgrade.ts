// Generated from Upgrade.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

import * as pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL from './../bbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2/DAVL';
import * as pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template from './../d14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662/DA/Internal/Template';
import * as pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL from './../edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';

export type UpgradeProposal_Accept = {
}
export const UpgradeProposal_Accept: daml.Serializable<UpgradeProposal_Accept> = ({
  decoder: () => jtv.object({
  }),
})

export type UpgradeProposal = {
  employee: daml.Party;
  company: daml.Party;
}
export const UpgradeProposal: daml.Template<UpgradeProposal, undefined, '6c6e8a208445b3429253fe0a28bbfea16ba0ec84b9220ef3b6c7ee7788f9dcef:Upgrade:UpgradeProposal'> & {
  Archive: daml.Choice<UpgradeProposal, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, undefined>;
  UpgradeProposal_Accept: daml.Choice<UpgradeProposal, UpgradeProposal_Accept, daml.ContractId<UpgradeAgreement>, undefined>;
} = {
  templateId: '6c6e8a208445b3429253fe0a28bbfea16ba0ec84b9220ef3b6c7ee7788f9dcef:Upgrade:UpgradeProposal',
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
  }),
  Archive: {
    template: () => UpgradeProposal,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
  UpgradeProposal_Accept: {
    template: () => UpgradeProposal,
    choiceName: 'UpgradeProposal_Accept',
    argumentDecoder: UpgradeProposal_Accept.decoder,
    resultDecoder: () => daml.ContractId(UpgradeAgreement).decoder(),
  },
};
daml.registerTemplate(UpgradeProposal);

export type UpgradeAgreement_UpgradeVacation = {
  employeeAgreementId: daml.ContractId<UpgradeAgreement>;
  vacationId: daml.ContractId<pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.Vacation>;
}
export const UpgradeAgreement_UpgradeVacation: daml.Serializable<UpgradeAgreement_UpgradeVacation> = ({
  decoder: () => jtv.object({
    employeeAgreementId: daml.ContractId(UpgradeAgreement).decoder(),
    vacationId: daml.ContractId(pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.Vacation).decoder(),
  }),
})

export type UpgradeAgreement_UpgradeVacationInternal = {
  boss: daml.Party;
  vacationId: daml.ContractId<pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.Vacation>;
}
export const UpgradeAgreement_UpgradeVacationInternal: daml.Serializable<UpgradeAgreement_UpgradeVacationInternal> = ({
  decoder: () => jtv.object({
    boss: daml.Party.decoder(),
    vacationId: daml.ContractId(pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.Vacation).decoder(),
  }),
})

export type UpgradeAgreement_UpgradeVacationRequest = {
  requestId: daml.ContractId<pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.VacationRequest>;
}
export const UpgradeAgreement_UpgradeVacationRequest: daml.Serializable<UpgradeAgreement_UpgradeVacationRequest> = ({
  decoder: () => jtv.object({
    requestId: daml.ContractId(pkgedb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7_DAVL.VacationRequest).decoder(),
  }),
})

export type UpgradeAgreement = {
  employee: daml.Party;
  company: daml.Party;
}
export const UpgradeAgreement: daml.Template<UpgradeAgreement, undefined, '6c6e8a208445b3429253fe0a28bbfea16ba0ec84b9220ef3b6c7ee7788f9dcef:Upgrade:UpgradeAgreement'> & {
  UpgradeAgreement_UpgradeVacationRequest: daml.Choice<UpgradeAgreement, UpgradeAgreement_UpgradeVacationRequest, daml.ContractId<pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.VacationRequest>, undefined>;
  UpgradeAgreement_UpgradeVacationInternal: daml.Choice<UpgradeAgreement, UpgradeAgreement_UpgradeVacationInternal, daml.ContractId<pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.Vacation>, undefined>;
  UpgradeAgreement_UpgradeVacation: daml.Choice<UpgradeAgreement, UpgradeAgreement_UpgradeVacation, daml.ContractId<pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.Vacation>, undefined>;
  Archive: daml.Choice<UpgradeAgreement, pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive, {}, undefined>;
} = {
  templateId: '6c6e8a208445b3429253fe0a28bbfea16ba0ec84b9220ef3b6c7ee7788f9dcef:Upgrade:UpgradeAgreement',
  keyDecoder: () => jtv.constant(undefined),
  decoder: () => jtv.object({
    employee: daml.Party.decoder(),
    company: daml.Party.decoder(),
  }),
  UpgradeAgreement_UpgradeVacationRequest: {
    template: () => UpgradeAgreement,
    choiceName: 'UpgradeAgreement_UpgradeVacationRequest',
    argumentDecoder: UpgradeAgreement_UpgradeVacationRequest.decoder,
    resultDecoder: () => daml.ContractId(pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.VacationRequest).decoder(),
  },
  UpgradeAgreement_UpgradeVacationInternal: {
    template: () => UpgradeAgreement,
    choiceName: 'UpgradeAgreement_UpgradeVacationInternal',
    argumentDecoder: UpgradeAgreement_UpgradeVacationInternal.decoder,
    resultDecoder: () => daml.ContractId(pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.Vacation).decoder(),
  },
  UpgradeAgreement_UpgradeVacation: {
    template: () => UpgradeAgreement,
    choiceName: 'UpgradeAgreement_UpgradeVacation',
    argumentDecoder: UpgradeAgreement_UpgradeVacation.decoder,
    resultDecoder: () => daml.ContractId(pkgbbbbfec7d278f3ac692e82fa94f1f4a322576abe043cdb7b490c67d1e10214d2_DAVL.Vacation).decoder(),
  },
  Archive: {
    template: () => UpgradeAgreement,
    choiceName: 'Archive',
    argumentDecoder: pkgd14e08374fc7197d6a0de468c968ae8ba3aadbf9315476fd39071831f5923662_DA_Internal_Template.Archive.decoder,
    resultDecoder: () => daml.Unit.decoder(),
  },
};
daml.registerTemplate(UpgradeAgreement);
