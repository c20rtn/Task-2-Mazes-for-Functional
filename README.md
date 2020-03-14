# Task Two - Maze Generator and Maze Solver

This assessment is an exercise in applying functional ideas to the problems of planning and pathfinding.

You must write your application in Clojure and may use any appropriate external libraries to help you, however you
must include citations as in-code comments identifying those libraries.

You are to submit your code and an academic poster on which you describe the implementation of your solution. You will be given advice on writing a poster later in the module

# Description

You should implement your solution as a client-server application. 

# Minimal Requirements

## First Task

* An application that can generate random mazes.
* These mazes must initially square.
* Your program must be capable of building square mazes in at least three different sizes.
* You may extend the generator to include triangular, circular or any other shape of two-dimensional maze that you wish.
* Maps are represented using structured data and held in a simple text file.

## Second Task

* A separate application that can solve mazes.
* The user can choose which of the maps will be attempted.
* The maze is loaded from the file.
* The solution (the route through the map) is written to the terminal

# Marking - Functionality

> ## Implement the minimal set of functionality
> ## __30__
> - Code implements appropriate domain functionality
> - Understanding of the domain is demonstrated within the code
> - The business logic is implemented in a sensible way

> ## Define and implement a suitable in-memory data structure to represent the maps.
> ## __10__
> - Define a data structure to represent the floor
> - The data structure is populated using appropriate code
> - The chosen data structure may be extensible

> ## Architecture
> ## __10__
> - The application has a clear separation between the map generator and map solver functions
> - Code is structured using modules etc. that are meaningful
> - Libraries, including 3rd party ones, are used to support the development of the application

> ## Data storage
> ## __5__
> - Maps are held in a file that can be accessed by the generator and the solver.
> - A simple NoSQL database is used to store the maps in more advanced solutions

> ## Networked client-server architecture
> ## __15__
> - The applications connect across a network
> - The client is able to connect and disconnect without breaking the server
> - Multiple clients can be connected concurrently

> ## Clarity and quality of the code
> ## __5__
> - Code is readable with meaningful names given to functions, data items etc.ode
> - Follows the appropriate idioms of the chosen language

# Marking - The Poster

> ## Structure, design and layout of the poster
> ## __5__
> - The layout is clear
> - Good use of colours and typography
> - Images, where used are meaningful

> ## Description of the Clojure language
> ## __5__
> - Understanding of the language is demonstrated
> - Its relevant merits are shown
> - Difficulties using it are identified
> - Criteria are set and provide a reasonable basis on which to build judgements
> - Conclusions are based on firm data

> ## Discussion of the data structures and algorithms
> ## __10__
> - The data structures are described
> - Algorithms are presented clearly and concisely
> - Code samples are used throughout
> - The discussion is technically advanced
> - The code legibly reflects the concepts of the domain


> ## Description of the approach to testing and evaluating the application
> ## __5__
> - The testing regime is explained
> - Testing is relevant to the problem
> - The system is shown to work in ways that go beyond those specified
> - The testing regime is explained
> - Testing is relevant to the problem
> - The system is shown to work in ways that go beyond those specified
