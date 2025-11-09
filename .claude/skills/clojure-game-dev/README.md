# Clojure Game Development Skill - Setup Complete

## 📁 File Structure

```
.claude/skills/clojure-game-dev/
├── SKILL.md           # Main skill file with YAML frontmatter
├── reference.md       # Detailed API and function reference
└── examples.md        # Concrete code examples
```

## ✅ What This Skill Provides

### Core Instructions (SKILL.md)
- REPL-driven interactive development workflow
- Smooth animation techniques using time-based math
- 3D rotation with quaternions
- Visual effects building (glows, shadows, outlines)
- Structural editing best practices

### API Reference (reference.md)
- Raylib drawing functions
- Animation frequencies and recommendations
- Vector types and quaternion formulas
- ECS entity access patterns
- Screen coordinate system
- Common mistakes and solutions

### Code Examples (examples.md)
- Animated cursor with hover effects
- Continuously rotating entities
- Pulsing animations with state
- Multi-layer visual effects
- Color interpolation
- Two-phase glow rings
- Frame-rate independent movement
- Conditional rendering
- Screen-centered UI
- Smooth state transitions

## 🎯 When Claude Will Use This Skill

The skill activates automatically when you ask about:
- Adding animations to game features
- Building visual effects (glows, shadows)
- Working with Clojure and Vybe game engine
- Creating interactive UI elements
- Rotating entities in 3D
- Real-time graphics programming
- Entity component system (ECS) management
- Raylib drawing operations

### Example Prompts That Trigger This Skill
- "Make the cursor animate smoothly"
- "Add a rotating TV to the game"
- "Create a glow effect on hover"
- "How do I animate something in Clojure?"
- "I need smooth color transitions"

## 📚 How to Reference the Skill

Files within the skill are automatically discovered by Claude:
- SKILL.md: Always active (main instructions)
- reference.md: Loaded on demand (detailed API info)
- examples.md: Loaded on demand (code patterns)

When asking a question, Claude will:
1. Read the main SKILL.md content
2. Pull additional details from reference.md if needed
3. Reference code patterns from examples.md for demonstrations

## 🔄 Future Updates

To update the skill:
1. Edit files in `.claude/skills/clojure-game-dev/`
2. Restart Claude Code to load changes
3. No need to commit immediately—changes take effect on restart

## 📝 Skill Naming Convention

- **Skill name**: `clojure-game-dev` (kebab-case, lowercase)
- **Skill type**: Project Skill (shared with team via `.claude/` in git)
- **Location**: `.claude/skills/clojure-game-dev/`

## ✨ Key Highlights

This skill captures practical knowledge from real game development work:
- Time-based animations (not random wobbles)
- Safe REPL workflow with immediate visual feedback
- Layered visual effects for professional appearance
- Quaternion-based 3D rotations
- Entity component system patterns
- Performance-conscious rendering

## 🚀 Using This Skill

Simply ask Claude about Clojure game development and it will:
1. Activate this skill automatically
2. Reference the correct patterns and APIs
3. Provide complete, tested examples
4. Explain best practices learned from real development

No need to manually invoke or reference—Claude handles it!

