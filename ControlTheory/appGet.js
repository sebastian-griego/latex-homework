app.get('/daily-question', async (req, res) => {
    const userId = req.user.id; // Assuming user is authenticated 
    
    // Fetch a random question from the database that the user hasn't seen
    const question = await Question.aggregate([
      { $match: { _id: { $nin: user.answeredQuestions } } }, 
      { $sample: { size: 1 } }
    ]);
    
    // Send the question and multiple choice answers to the app
    res.json({
      questionId: question._id,
      questionText: question.text,
      answers: question.answers
    });
    
    // Add the question to the user's answered list
    await User.updateOne(
      { id: userId },
      { $push: { answeredQuestions: question._id } } 
    );
  });