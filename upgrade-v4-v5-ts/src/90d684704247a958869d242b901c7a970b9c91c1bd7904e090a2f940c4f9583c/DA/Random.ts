// Generated from DA/Random.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Minstd = 
  |  { tag: 'Minstd'; value: daml.Int }
export const Minstd:
  daml.Serializable<Minstd> & {
  } = ({
  decoder: () => jtv.oneOf<Minstd>(
    jtv.object({tag: jtv.constant('Minstd'), value: jtv.lazy(() => daml.Int.decoder())}),
  ),
});
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<Minstd>(Minstd)
