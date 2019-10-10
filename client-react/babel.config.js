module.exports = api => {
  api.cache(true)

  return {
    presets: [
      [
        '@babel/env',
        {
          useBuiltIns: 'entry',
          corejs: '3.0.0'
        }
      ],
      '@babel/typescript',
      '@babel/react'
    ],
    plugins: ['@babel/proposal-class-properties']
  }
}
