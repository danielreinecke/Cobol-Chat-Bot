// Import React hooks for state management and side effects
import React, { useState, useRef, useEffect } from 'react';
// Import icons from lucide-react library for UI elements
import { Send, BookOpen, Code, AlertCircle, Sparkles } from 'lucide-react';

/**
 * CobolTutorChat - Main chat interface component for the COBOL AI Tutor
 * 
 * This component provides a chat interface for students to interact with
 * the COBOL AI tutor. It handles message display, user input, API communication,
 * and UI states (loading, errors, etc.).
 * 
 * @component
 */
export default function CobolTutorChat() {
  // State: Array of message objects with role ('user' or 'assistant') and content
  // Initialize with a welcome message from the assistant
  const [messages, setMessages] = useState([
    {
      role: 'assistant',
      content: 'Hello! I\'m your COBOL and JCL tutor. I can help you understand COBOL structure, debug errors, explain mainframe concepts, and more. What would you like to learn today?'
    }
  ]);
  
  // State: Current user input text
  const [input, setInput] = useState('');
  
  // State: Loading indicator for when waiting for API response
  const [isLoading, setIsLoading] = useState(false);
  
  // Ref: Reference to the end of messages list for auto-scrolling
  const messagesEndRef = useRef(null);

  /**
   * Scrolls the chat window to the bottom to show the latest message
   * Uses smooth scrolling for better UX
   */
  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  /**
   * Effect: Auto-scroll to bottom whenever messages array changes
   * This ensures users always see the latest message
   */
  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  /**
   * Handles form submission when user sends a message
   * 
   * Flow:
   * 1. Prevent default form submission
   * 2. Validate input is not empty
   * 3. Add user message to chat
   * 4. Send request to backend API
   * 5. Display assistant's response
   * 6. Handle any errors
   * 
   * @param {Event} e - Form submission event
   */
  const handleSubmit = async (e) => {
    e.preventDefault(); // Prevent page reload on form submit
    if (!input.trim()) return; // Don't send empty messages

    // Create user message object and add to messages array
    const userMessage = { role: 'user', content: input };
    setMessages(prev => [...prev, userMessage]);
    
    // Clear input field and set loading state
    setInput('');
    setIsLoading(true);

    try {
      // Send POST request to Flask backend API
      // TODO: Replace this URL with your actual backend endpoint
      const response = await fetch('http://localhost:5000/api/chat', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          message: input,                    // Current user message
          conversation_history: messages     // Previous messages for context
        }),
      });

      // Parse JSON response from API
      const data = await response.json();
      
      // Add assistant's response to messages array
      setMessages(prev => [...prev, {
        role: 'assistant',
        content: data.response || 'Sorry, I encountered an error. Please try again.'
      }]);
    } catch (error) {
      // Handle network errors or API failures
      console.error('Error:', error);
      setMessages(prev => [...prev, {
        role: 'assistant',
        content: 'Sorry, I\'m having trouble connecting to the server. Please check that the backend is running.'
      }]);
    } finally {
      // Always reset loading state when done
      setIsLoading(false);
    }
  };

  /**
   * Quick prompt buttons - Common questions students can click
   * Each prompt has:
   * - icon: Lucide React icon component
   * - text: Display text for the button
   * - query: Full question to send to the API
   */
  const quickPrompts = [
    { icon: Code, text: 'Explain COBOL structure', query: 'Can you explain the structure of a COBOL program with DIVISIONS, SECTIONS, and PARAGRAPHS?' },
    { icon: AlertCircle, text: 'Debug error codes', query: 'I got error code S0C7 in my program. What does it mean?' },
    { icon: BookOpen, text: 'Learn DATA DIVISION', query: 'Explain what the DATA DIVISION does and how to use it.' },
  ];

  /**
   * Handles clicking a quick prompt button
   * Fills the input field with the pre-written query
   * 
   * @param {string} query - The question to populate in the input field
   */
  const handleQuickPrompt = (query) => {
    setInput(query);
  };

  return (
    // Main container - full screen height with gradient background
    <div className="flex flex-col h-screen bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900">
      
      {/* ==================== HEADER SECTION ==================== */}
      <div className="bg-slate-800/50 backdrop-blur-sm border-b border-slate-700/50 shadow-lg">
        <div className="max-w-5xl mx-auto px-4 py-4">
          <div className="flex items-center gap-3">
            {/* Logo/Icon */}
            <div className="bg-gradient-to-br from-blue-500 to-cyan-500 p-2 rounded-lg shadow-lg">
              <Sparkles className="w-6 h-6 text-white" />
            </div>
            {/* Title and subtitle */}
            <div>
              <h1 className="text-xl font-bold text-white">COBOL AI Tutor</h1>
              <p className="text-sm text-slate-400">Learn mainframe programming with AI assistance</p>
            </div>
          </div>
        </div>
      </div>

      {/* ==================== MESSAGES SECTION ==================== */}
      <div className="flex-1 overflow-y-auto px-4 py-6">
        <div className="max-w-5xl mx-auto space-y-6">
          {/* Map through all messages and display them */}
          {messages.map((message, index) => (
            <div
              key={index}
              className={`flex ${message.role === 'user' ? 'justify-end' : 'justify-start'}`}
            >
              <div
                className={`max-w-3xl rounded-2xl px-5 py-4 shadow-lg ${
                  message.role === 'user'
                    ? 'bg-gradient-to-br from-blue-600 to-cyan-600 text-white'  // User messages: blue gradient
                    : 'bg-slate-800/80 backdrop-blur-sm text-slate-100 border border-slate-700/50'  // Assistant messages: dark slate
                }`}
              >
                {/* Pre-wrap preserves whitespace and line breaks from the response */}
                <div className="whitespace-pre-wrap break-words">{message.content}</div>
              </div>
            </div>
          ))}

          {/* Loading indicator - shown while waiting for API response */}
          {isLoading && (
            <div className="flex justify-start">
              <div className="bg-slate-800/80 backdrop-blur-sm border border-slate-700/50 rounded-2xl px-5 py-4 shadow-lg">
                <div className="flex items-center gap-2">
                  {/* Three animated dots */}
                  <div className="flex space-x-1">
                    <div className="w-2 h-2 bg-cyan-500 rounded-full animate-bounce" style={{ animationDelay: '0ms' }}></div>
                    <div className="w-2 h-2 bg-cyan-500 rounded-full animate-bounce" style={{ animationDelay: '150ms' }}></div>
                    <div className="w-2 h-2 bg-cyan-500 rounded-full animate-bounce" style={{ animationDelay: '300ms' }}></div>
                  </div>
                  <span className="text-slate-400 text-sm">Thinking...</span>
                </div>
              </div>
            </div>
          )}
          {/* Invisible div for auto-scroll target */}
          <div ref={messagesEndRef} />
        </div>
      </div>

      {/* ==================== QUICK PROMPTS SECTION ==================== */}
      {/* Only show quick prompts when conversation is new (1 message = initial greeting) */}
      {messages.length <= 1 && (
        <div className="px-4 pb-4">
          <div className="max-w-5xl mx-auto">
            <p className="text-sm text-slate-400 mb-3 text-center">Try asking:</p>
            <div className="flex flex-wrap gap-2 justify-center">
              {/* Render a button for each quick prompt */}
              {quickPrompts.map((prompt, index) => {
                const Icon = prompt.icon;
                return (
                  <button
                    key={index}
                    onClick={() => handleQuickPrompt(prompt.query)}
                    className="flex items-center gap-2 px-4 py-2 bg-slate-800/60 hover:bg-slate-700/60 text-slate-300 rounded-xl border border-slate-700/50 transition-all hover:scale-105 shadow-lg"
                  >
                    <Icon className="w-4 h-4" />
                    <span className="text-sm">{prompt.text}</span>
                  </button>
                );
              })}
            </div>
          </div>
        </div>
      )}

      {/* ==================== INPUT SECTION ==================== */}
      <div className="bg-slate-800/50 backdrop-blur-sm border-t border-slate-700/50 px-4 py-4">
        <div className="max-w-5xl mx-auto">
          <form onSubmit={handleSubmit} className="flex gap-3">
            {/* Text input field */}
            <input
              type="text"
              value={input}
              onChange={(e) => setInput(e.target.value)}
              placeholder="Ask about COBOL, JCL, or mainframe concepts..."
              className="flex-1 px-5 py-3 bg-slate-900/80 backdrop-blur-sm border border-slate-700/50 rounded-xl text-white placeholder-slate-500 focus:outline-none focus:ring-2 focus:ring-cyan-500/50 focus:border-cyan-500/50 shadow-lg"
              disabled={isLoading}  // Disable input while loading
            />
            {/* Submit button */}
            <button
              type="submit"
              disabled={isLoading || !input.trim()}  // Disable if loading or input is empty
              className="px-6 py-3 bg-gradient-to-r from-blue-600 to-cyan-600 hover:from-blue-500 hover:to-cyan-500 disabled:from-slate-700 disabled:to-slate-700 disabled:cursor-not-allowed text-white rounded-xl font-medium transition-all shadow-lg hover:scale-105 disabled:scale-100 flex items-center gap-2"
            >
              <Send className="w-5 h-5" />
              {/* Hide text on small screens, show on larger screens */}
              <span className="hidden sm:inline">Send</span>
            </button>
          </form>
        </div>
      </div>
    </div>
  );
}
