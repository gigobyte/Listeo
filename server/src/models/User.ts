import * as mongoose from 'mongoose'
import * as bcrypt from 'bcrypt-nodejs'

const USERNAME_REGEX = /^[a-zA-Z0-9]{4,}$/
const PASSWORD_REGEX = /^.{6,}$/

interface User extends mongoose.Document {
    email: string
    username: string
    password: string
    createdAt: number
}

const UserSchema = new mongoose.Schema({
    email: { type: String, required: true, unique: true },
    username: { type: String, required: true, unique: true, match: USERNAME_REGEX },
    password: { type: String, required: true, match: PASSWORD_REGEX },
    createdAt: { type: Date, default: Date.now() }
})

UserSchema.pre<User>('save', function (next) {
    if (!this.isNew) {
        return next()
    }

    bcrypt.hash(this.password, bcrypt.genSaltSync(10), () => { }, (err, hash) => {
        if (err) {
            return next(err)
        }

        this.password = hash
        next()
    })
})

export const User = mongoose.model('User', UserSchema)