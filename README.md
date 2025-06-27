# 🎓 Skillsync - Skills Transfer DAO

> **Empowering immigrants to validate & teach their homeland skills**

Skillsync is a decentralized platform that enables immigrants to monetize their expertise by teaching skills from their homeland while building trust through community validation.

## ✨ Features

- 👨‍🏫 **Teacher Registration** - Register as a skill teacher with profile and bio
- 🎯 **Skill Listing** - Add skills with descriptions, categories, and hourly rates
- ✅ **Community Validation** - Stake-based skill validation system
- 📚 **Lesson Booking** - Students can book and pay for lessons
- ⭐ **Rating System** - Mutual rating between teachers and students
- 💰 **Secure Payments** - Built-in escrow and payment system
- 🏦 **Balance Management** - Deposit/withdraw STX tokens

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for transactions

### Contract Deployment

```bash
clarinet deploy --network testnet
```

## 📖 Usage Instructions

### For Teachers 👩‍🏫

1. **Register as Teacher**
   ```clarity
   (contract-call? .skillsync register-teacher "John Doe" "Expert software engineer from India")
   ```

2. **Add a Skill**
   ```clarity
   (contract-call? .skillsync add-skill 
     "Python Programming" 
     "Learn Python from basics to advanced" 
     "Programming" 
     u50000000) ;; 50 STX per hour
   ```

3. **Complete Lessons**
   ```clarity
   (contract-call? .skillsync complete-lesson u1)
   ```

### For Students 🎓

1. **Deposit Funds**
   ```clarity
   (contract-call? .skillsync deposit-funds u100000000) ;; 100 STX
   ```

2. **Book a Lesson**
   ```clarity
   (contract-call? .skillsync book-lesson u1 u2 u1000) ;; skill-id, 2 hours, block height
   ```

3. **Rate Teacher**
   ```clarity
   (contract-call? .skillsync rate-lesson u1 u5 true) ;; lesson-id, 5 stars, is-student
   ```

### For Validators 🔍

1. **Validate Skills** (requires stake)
   ```clarity
   (contract-call? .skillsync validate-skill u1) ;; skill-id
   ```

## 🔧 Contract Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `register-teacher` | Register as a skill teacher |
| `add-skill` | List a new skill for teaching |
| `validate-skill` | Validate a teacher's skill (requires stake) |
| `book-lesson` | Book and pay for a lesson |
| `complete-lesson` | Mark lesson as completed (teacher only) |
| `rate-lesson` | Rate a completed lesson |
| `deposit-funds` | Add STX to your balance |
| `withdraw-funds` | Withdraw STX from your balance |
| `toggle-skill-status` | Activate/deactivate a skill |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-teacher` | Get teacher profile information |
| `get-skill` | Get skill details by ID |
| `get-lesson` | Get lesson information by ID |
| `get-user-balance` | Check user's STX balance |
| `get-teacher-skills` | List all skills by a teacher |
| `get-skill-validation` | Check validation status |

## 💡 How It Works

1. **🔐 Registration**: Immigrants register as teachers with their background
2. **📝 Skill Listing**: Teachers list their skills with pricing
3. **✅ Validation**: Community members stake tokens to validate skills
4. **💰 Booking**: Students book lessons with escrow payment
5. **🎯 Teaching**: Teachers conduct lessons and mark as complete
6. **⭐ Rating**: Both parties rate each other for reputation building

## 🏗️ Data Structure

### Teachers
- Name, bio, reputation score
- Total earnings and skills count
- Join date

### Skills
- Title, description, category
- Hourly rate and validation count
- Rating and lesson statistics

### Lessons
- Skill reference and participants
- Duration, cost, and scheduling
- Status and ratings

## ⚙️ Configuration

- **Platform Fee**: 5% (adjustable by contract owner)
- **Minimum Validation Stake**: 1 STX
- **Rating Scale**: 1-5 stars
- **Maximum Skills per Teacher**: 100

## 🛡️ Security Features

- Stake-based validation system
- Escrow payment protection
- Authorization checks for all actions
- Balance verification before transactions
- Rate limiting and validation

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Test your changes with Clarinet
4. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.


