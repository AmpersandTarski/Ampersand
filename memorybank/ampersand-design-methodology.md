# Systematic Approach for Designing in Ampersand

## Overview
This document describes a systematic approach for designing and developing Ampersand applications.

## **Step 1: Requirements Analysis**

### **What to do:**
- Thoroughly analyze user story and break it down into concrete functional requirements.
- Get data samples to analyze production data.
- Identify core entities and their relationships
- Determine business rules and constraints
- Create interfaces so that overviews of all tables are available.
- Create interfaces so that the user can edit a single record of each concept
- Ensure everything has a purpose, so users and AI assistants can understand why things exist.
- Validate:



## **Step 4: Derive Conceptual Model**

### **From interface to relations:**
By analyzing the interface I could exactly determine which relations were needed:

**From interface field → Derive relation:**
- `changeTitle` → `changeTitle[Change*Title] [UNI,TOT]`
- `concernsRequirement` → `concernsRequirement[Change*Requirement] [UNI,TOT]` 
- `changeProducts` → `changeProducts[Change*Product] [TOT]`

### **Determine multiplicities:**
- **[UNI]** for 1:1 relations (but be sparing with TOT)
- **No TOT constraint** unless you have a clear reason
- **Think about user experience** - TOT forces immediate completion

**CRUCIAL RULES:**
1. **Never use TOT/SUR/UNI unless you know why** you set the requirement
2. **With TOT/SUR always ask:** Do you really want to require completion? What does this do for user experience?
3. **Distinguish:** "immediately required completion" vs "eventually required completion"

## **Step 5: Patterns and Logical Grouping**

### **Pattern division:**
- `ChangeData` - all basic relations of Change
- `AnnotationData` - properties of Annotation concept  
- `EvidenceData` - properties of Evidence concept

### **Why use patterns:**
- Logical grouping of related relations
- Easier maintenance and understanding
- Follows Ampersand best practices
- Helps with modularity

## **Step 6: Validation Rules and Business Logic**

### **Rules derived from requirements:**
```ampersand
RULE "Change must have at least one product":
  I[Change] |- changeProducts;changeProducts~
```

### **Types of rules:**
- **Integrity rules** - data consistency
- **Business rules** - domain-specific rules  
- **Workflow rules** - process control

## **Step 7: Views and User Experience**

### **Views for readability:**
```ampersand
VIEW Change: Change DEFAULT
  { title: changeTitle
  , dash: TXT " - "  
  , requirement: concernsRequirement;requirementText
  }
```

### **Populations for ease of use:**
```ampersand
POPULATION EvidenceType CONTAINS
  [ "Document", "URL/Link", "Email" ]
```

## **Step 8: Iteration and Refinement**

### **Checklist for review:**
- [ ] Does the documentation describe (or refer to) the problem this script solves?
- [ ] All user story elements implemented?
- [ ] Which excess functionality is defined?
- [ ] Are there concepts, relations, rules, interfaces, patterns, or fields in interfaces of which you don't know why it exists?
- [ ] Are there relations with a similar purpose or meaning? Would it make sense to combine them in one relation?
- [ ] Are there concepts with overlapping relations? Would it be useful to combine them using specialization?
- [ ] Existing concepts optimally reused?
- [ ] Relations have correct multiplicities?
- [ ] Interface logical and user-friendly?
- [ ] Validation rules cover main constraints?
- [ ] Are there any inconsistencies?
- [ ] Are there relations not covered by any consistency rule? Does the documentation explain why?
- [ ] Is all data in the database of the prototype visible in overview interfaces?
- [ ] Is all data editable?


## **General Design Principles**

### **1. Start Simple, Build Out**
- Begin with minimal viable interface
- Gradually add complexity
- Test early and often

### **2. Reuse First, Create Later**
- Explore existing codebase thoroughly
- Reuse concepts and patterns where possible
- Only add new concepts when truly needed

### **3. Interface-Driven Design**
- Begin with user interface
- Derive data model from interface
- Validate design against user need

### **4. Patterns for Structure**
- Group related relations in patterns
- Use meaningful pattern names
- Document with PURPOSE statements

### **5. Rules for Quality**
- Define business rules explicitly
- Use clear rule names and messages
- Test edge cases and error scenarios

### **6. Critical Evaluation Principle (NEW)**
- **Ask for each field:** "What is this field for?"
- **Ask for each relation:** "What is this relation for?"
- **If you don't have a good answer:** Remove it
- **Simplicity over complexity:** Rather too simple than too complex
- **Write PURPOSE statements:** Document why each relation exists for every relation

## **Tips for Educational Material**

### **Teaching sequence:**
1. **Concepts and Relations** - Basic Ampersand knowledge
2. **Interface Design** - User interaction
3. **Pattern Thinking** - Structure and organization
4. **Rule Engineering** - Business logic and validation
5. **Integration** - Cooperation between modules

### **Hands-on exercises:**
- Give students a user story, let them follow this methodology step-by-step
- Start with simple scenarios (e.g. library system)
- Gradually build complexity
- Let students analyze and extend existing code

### **Common mistakes:**
- Starting directly with data model instead of interface
- Creating too many new concepts instead of reusing
- **Using TOT/SUR constraints without clear reason** (NEW)
- **Designing too complex without critical evaluation** (NEW)
- Unclear pattern and rule naming
- Creating interfaces without thinking about user experience with constraints

### **Proven improvements from practice:**
- **Away from `[UNI,TOT]` towards `[UNI]`** - Better user experience
- **Simplify interface** - Omissions are often better than additions
- **Critically evaluate every relation** - "What is this for?" principle
- **Use PURPOSE statements** - Document why every relation is needed

This methodology can be applied to virtually any Ampersand project and helps students design systematically and thoughtfully.
