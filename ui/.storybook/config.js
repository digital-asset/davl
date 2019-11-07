import { configure } from '@storybook/react';
import 'semantic-ui-css/semantic.min.css';
import '../src/index.css';

const req = require.context('../src', true, /.stories.tsx$/);
function loadStories() {
  req.keys().forEach(req);
}
configure(loadStories, module);
