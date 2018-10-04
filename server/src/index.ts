import * as express from 'express'
import * as bodyParser from 'body-parser'
import * as cors from 'cors'
import * as mongoose from 'mongoose'

mongoose.connect('mongodb://localhost/listeodb', { useNewUrlParser: true })

const app = express()

app.use(bodyParser.json())
app.use(cors())

const port = process.env.PORT || 8081
app.listen(port)

console.log(`Started on ${new Date().toDateString()} and port ${port}...`)