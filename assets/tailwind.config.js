module.exports = {
   mode: 'jit',
   purge: [
     './app.js',
     '../apps/styx_web/templates/*.dtl',
     '../apps/styx_web/src/**/*.erl'
   ],
   darkMode: 'media',
   theme: {
   },
   variants: {
     extend: {},
   },
   plugins: [],
 }

