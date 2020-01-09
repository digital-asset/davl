// Generated from DA/Validation.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

import * as DA_NonEmpty from '../DA/NonEmpty';

export type Validation<a9s4, a9s5> = 
  |  { tag: 'Errors'; value: DA_NonEmpty.NonEmpty<a9s4> }
  |  { tag: 'Success'; value: a9s5 }
export const Validation = <a9s4, a9s5>(a9s4: daml.Serializable<a9s4>, a9s5: daml.Serializable<a9s5>): daml.Serializable<Validation<a9s4, a9s5>> => ({
  decoder: () => jtv.oneOf(
    jtv.object<Validation<a9s4, a9s5>>({tag: jtv.constant('Errors'), value: jtv.lazy(() => DA_NonEmpty.NonEmpty(a9s4).decoder())}),
    jtv.object<Validation<a9s4, a9s5>>({tag: jtv.constant('Success'), value: jtv.lazy(() => a9s5.decoder())}),
  )
});
