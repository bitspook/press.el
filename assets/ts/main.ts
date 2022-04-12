import mixpanel from 'mixpanel-browser';

const mpToken = "0f28a64d9f8bce370006d36e1e2e3f61";

mixpanel.init(mpToken, { secure_cookie: true });
mixpanel.track('Page Load');
