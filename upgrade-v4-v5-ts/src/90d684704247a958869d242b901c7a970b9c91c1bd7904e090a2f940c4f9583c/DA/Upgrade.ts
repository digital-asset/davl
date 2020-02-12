// Generated from DA/Upgrade.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type MetaEquiv<m1_a4VP, m2_a4VQ> = {
}
export const MetaEquiv = <m1_a4VP, m2_a4VQ>(m1_a4VP: daml.Serializable<m1_a4VP>, m2_a4VQ: daml.Serializable<m2_a4VQ>): daml.Serializable<MetaEquiv<m1_a4VP, m2_a4VQ>> => ({
  decoder: () => jtv.object({
  }),
})
