// Generated from DA/Validation/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

import * as pkge22bce619ae24ca3b8e6519281cb5a33b64b3190cc763248b4c3f9ad5087a92c_DA_NonEmpty_Types from '../../../e22bce619ae24ca3b8e6519281cb5a33b64b3190cc763248b4c3f9ad5087a92c/DA/NonEmpty/Types';

export type Validation<errs, a> = 
  |  { tag: 'Errors'; value: pkge22bce619ae24ca3b8e6519281cb5a33b64b3190cc763248b4c3f9ad5087a92c_DA_NonEmpty_Types.NonEmpty<errs> }
  |  { tag: 'Success'; value: a }
export const Validation = <errs, a>(errs: daml.Serializable<errs>, a: daml.Serializable<a>): daml.Serializable<Validation<errs, a>> => ({
  decoder: () => jtv.oneOf<Validation<errs, a>>(
    jtv.object({tag: jtv.constant('Errors'), value: jtv.lazy(() => pkge22bce619ae24ca3b8e6519281cb5a33b64b3190cc763248b4c3f9ad5087a92c_DA_NonEmpty_Types.NonEmpty(errs).decoder())}),
    jtv.object({tag: jtv.constant('Success'), value: jtv.lazy(() => a.decoder())}),
  ),
});
