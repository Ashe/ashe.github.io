const defaultTheme = require('tailwindcss/defaultTheme')
const colors = require('tailwindcss/colors')

module.exports = {
  darkMode: 'class',
  purge: {
    content: [
    './data/**/*.html',
    ],
  },
  theme: {
    extend: {
      fontFamily: {
        sans: [
          'Lato',
          defaultTheme.fontFamily.sans,
        ],
        serif: [
          "Merriweather",
          "Roboto Slab",
          defaultTheme.fontFamily.serif
        ],
        'standout': [
            'Fira Code', 'monospace'
        ]
      },
      fontSize: {
        '2xs': ".5rem",
        'xs': '.75rem',
        'sm': '.875rem',
        body: ["15px", "22.5px"],
        'lg': '1.125rem',
        'xl': '1.25rem',
        '2xl': '1.375rem',
       '3xl': '1.625rem',
       '4xl': '2.25rem',
        '5xl': '3rem',
        '6xl': '4rem',
       '7xl': '5rem',

        'lead': '1.125rem'
      },
      colors: {
        textBlack: "#252525",
        textWhite: "#cbd9f4",
        muted: "#f5f5f5",
        emphasis: "#444",
        dark: "#252525",
        night: "#172136",
        mutedNight: "#273146",
        darkNight: "#101429",
        hr: "#e5e5e5",
        hrNight: "#2b3a59",
        code: "var(--bg-code)",
        highlight: "#5d8c91",
        highlightNight: "#8fd8e0",
          
        note: '#a5d2ff',
        noteNight: '#33608c',
        help: '#85fcd2',
        helpNight: '#24604b',
        warning: '#ffbc89',
        warningNight: '#897e00',
        danger: '#ff8298',
        dangerNight: '#661423',
        repo: '#ac9ddd',
        repoNight: '#32255e',

        // primary: 'var(--color-primary)',
        // secondary: 'var(--color-secondary)',
        // heading: 'var(--color-heading)',
        // body: 'var(--color-body)'
      },
      textColor: {
        black: "#252525",
        white: "#cbd9f4",
        muted: "#999",
        mutedDark: "#b5b5b5",
        mutedNight: "#677186",
        emphasizedNight: "#8cabe6",
        code: "#3d545b",
      },
      container: {
        center: true,
        padding: {
          DEFAULT: '1rem',
          sm: '2rem',
          lg: '3rem',
          xl: '4rem',
          '2xl': '5rem',
        },
      },
      height: {
        xl: '32rem',
        '2xl': '40rem',
        '3xl': '48rem',
       },
    },
  },
  variants: {},
  plugins: [],
}
