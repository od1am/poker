// src/main.rs
use itertools::Itertools;
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;
use std::io::{self};


// Cards Values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Suit {
    Hearts,
    Diamonds,
    Clubs,
    Spades,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Rank {
    Two = 2,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(Debug)]
enum Action {
    Fold,
    Call,
    Raise(u32),
}

#[derive(Debug)]
struct AIDecision {
    action: Action,
    confidence: f32,
}

trait PokerAI {
    fn decide_action(&self, game_state: &GameState) -> AIDecision;
}

struct BasicPokerAI;

impl PokerAI for BasicPokerAI {
    fn decide_action(&self, game_state: &GameState) -> AIDecision {
        let hand_strength = self.evaluate_hand_strength(game_state);
        let pot_odds = self.calculate_pot_odds(game_state);
        
        match hand_strength {
            0.8..=1.0 => AIDecision { 
                action: Action::Raise(game_state.current_bet * 2),
                confidence: hand_strength 
            },
            0.6..=0.79 => AIDecision { 
                action: Action::Call,
                confidence: hand_strength 
            },
            _ => {
                if pot_odds > hand_strength {
                    AIDecision { 
                        action: Action::Call,
                        confidence: pot_odds 
                    }
                } else {
                    AIDecision { 
                        action: Action::Fold,
                        confidence: 1.0 - hand_strength 
                    }
                }
            }
        }
    }
}

impl BasicPokerAI {
    fn evaluate_hand_strength(&self, game_state: &GameState) -> f32 {
        // Basic hand strength evaluation
        let hand_rank = evaluate_hand(&game_state.current_hand);
        match hand_rank {
            HandRank::RoyalFlush => 1.0,
            HandRank::StraightFlush(_) => 0.95,
            HandRank::FourOfAKind(_, _) => 0.9,
            HandRank::FullHouse(_, _) => 0.8,
            HandRank::Flush(_) => 0.7,
            HandRank::Straight(_) => 0.6,
            HandRank::ThreeOfAKind(_, _) => 0.5,
            HandRank::TwoPair(_, _, _) => 0.4,
            HandRank::OnePair(_, _) => 0.3,
            HandRank::HighCard(_) => 0.1,
        }
    }

    fn calculate_pot_odds(&self, game_state: &GameState) -> f32 {
        let call_amount = game_state.current_bet;
        if call_amount == 0 {
            return 1.0;
        }
        let pot_odds = game_state.pot as f32 / call_amount as f32;
        (pot_odds / (pot_odds + 1.0)).min(1.0)
    }
}

// Add this struct to represent the game state
struct GameState {
    current_hand: Vec<Card>,
    community_cards: Vec<Card>,
    pot: u32,
    current_bet: u32,
    player_chips: u32,
    round: BettingRound,
}

#[derive(Debug, Clone, Copy)]
enum BettingRound {
    PreFlop,
    Flop,
    Turn,
    River,
}

fn evaluate_hand(cards: &[Card]) -> HandRank {
    // Sort cards by rank in descending order
    let mut sorted_cards = cards.to_vec();
    sorted_cards.sort_by(|a, b| b.rank.cmp(&a.rank));

    let is_flush = check_flush(&sorted_cards);
    let is_straight = check_straight(&sorted_cards);
    let rank_counts = count_ranks(&sorted_cards);

    // Check for Royal Flush and Straight Flush
    if is_flush && is_straight {
        if sorted_cards[0].rank == Rank::Ace && sorted_cards[1].rank == Rank::King {
            return HandRank::RoyalFlush;
        } else {
            return HandRank::StraightFlush(sorted_cards[0].rank);
        }
    }

    // Check for Four of a Kind
    if let Some((quad_rank, kicker)) = check_four_of_a_kind(&rank_counts, &sorted_cards) {
        return HandRank::FourOfAKind(quad_rank, kicker);
    }

    // Check for Full House
    if let Some((trip_rank, pair_rank)) = check_full_house(&rank_counts) {
        return HandRank::FullHouse(trip_rank, pair_rank);
    }

    // Check for Flush
    if is_flush {
        let ranks = sorted_cards.iter().map(|c| c.rank).collect();
        return HandRank::Flush(ranks);
    }

    // Check for Straight
    if is_straight {
        return HandRank::Straight(sorted_cards[0].rank);
    }

    // Check for Three of a Kind
    if let Some((trip_rank, kickers)) = check_three_of_a_kind(&rank_counts, &sorted_cards) {
        return HandRank::ThreeOfAKind(trip_rank, kickers);
    }

    // Check for Two Pair
    if let Some((high_pair, low_pair, kicker)) = check_two_pair(&rank_counts, &sorted_cards) {
        return HandRank::TwoPair(high_pair, low_pair, kicker);
    }

    // Check for One Pair
    if let Some((pair_rank, kickers)) = check_one_pair(&rank_counts, &sorted_cards) {
        return HandRank::OnePair(pair_rank, kickers);
    }

    // High Card
    let ranks = sorted_cards.iter().map(|c| c.rank).collect();
    HandRank::HighCard(ranks)
}
fn count_ranks(cards: &[Card]) -> HashMap<Rank, u32> {
    let mut counts = HashMap::new();
    for card in cards {
        *counts.entry(card.rank).or_insert(0) += 1;
    }
    counts
}
fn check_flush(cards: &[Card]) -> bool {
    let suit = cards[0].suit;
    cards.iter().all(|card| card.suit == suit)
}
fn check_straight(cards: &[Card]) -> bool {
    let mut ranks: Vec<usize> = cards.iter().map(|c| c.rank as usize).collect();
    ranks.sort_unstable();
    ranks.dedup(); // Remove duplicates

    // Handle Ace as both high and low
    if ranks.contains(&(Rank::Ace as usize)) && ranks.contains(&(Rank::Two as usize)) {
        let mut low_ace_ranks = ranks.clone();
        low_ace_ranks.push(1); // Ace as 1
        low_ace_ranks.sort_unstable();

        return is_consecutive(&low_ace_ranks);
    }

    is_consecutive(&ranks)
}

fn is_consecutive(ranks: &[usize]) -> bool {
    if ranks.len() < 5 {
        return false;
    }
    for window in ranks.windows(5) {
        if window[4] - window[0] == 4
            && window
                .iter()
                .zip(window.iter().skip(1))
                .all(|(a, b)| b - a == 1)
        {
            return true;
        }
    }
    false
}

fn check_four_of_a_kind(rank_counts: &HashMap<Rank, u32>, cards: &[Card]) -> Option<(Rank, Rank)> {
    for (&rank, &count) in rank_counts {
        if count == 4 {
            let kicker = cards
                .iter()
                .filter(|&c| c.rank != rank)
                .map(|c| c.rank)
                .max()
                .unwrap();
            return Some((rank, kicker));
        }
    }
    None
}

fn check_full_house(rank_counts: &HashMap<Rank, u32>) -> Option<(Rank, Rank)> {
    let mut trip_rank = None;
    let mut pair_rank = None;

    for (&rank, &count) in rank_counts {
        if count == 3 {
            trip_rank = Some(rank);
        } else if count == 2 {
            if let Some(existing_pair) = pair_rank {
                if rank > existing_pair {
                    pair_rank = Some(rank);
                }
            } else {
                pair_rank = Some(rank);
            }
        }
    }

    if trip_rank.is_some() && pair_rank.is_some() {
        return Some((trip_rank.unwrap(), pair_rank.unwrap()));
    }
    None
}

fn check_three_of_a_kind(
    rank_counts: &HashMap<Rank, u32>,
    cards: &[Card],
) -> Option<(Rank, Vec<Rank>)> {
    for (&rank, &count) in rank_counts {
        if count == 3 {
            let mut kickers = cards
                .iter()
                .filter(|&c| c.rank != rank)
                .map(|c| c.rank)
                .collect::<Vec<_>>();
            kickers.sort_by(|a, b| b.cmp(a));
            kickers.truncate(2);
            return Some((rank, kickers));
        }
    }
    None
}

fn check_two_pair(rank_counts: &HashMap<Rank, u32>, cards: &[Card]) -> Option<(Rank, Rank, Rank)> {
    let mut pairs = rank_counts
        .iter()
        .filter(|&(_, &count)| count == 2)
        .map(|(&rank, _)| rank)
        .collect::<Vec<_>>();

    if pairs.len() >= 2 {
        pairs.sort_by(|a, b| b.cmp(a));
        let kicker = cards
            .iter()
            .filter(|&c| c.rank != pairs[0] && c.rank != pairs[1])
            .map(|c| c.rank)
            .max()
            .unwrap();
        return Some((pairs[0], pairs[1], kicker));
    }
    None
}

fn check_one_pair(rank_counts: &HashMap<Rank, u32>, cards: &[Card]) -> Option<(Rank, Vec<Rank>)> {
    let mut pair_rank = None;
    for (&rank, &count) in rank_counts {
        if count == 2 {
            pair_rank = Some(rank);
            break;
        }
    }
    if let Some(rank) = pair_rank {
        let mut kickers = cards
            .iter()
            .filter(|&c| c.rank != rank)
            .map(|c| c.rank)
            .collect::<Vec<_>>();
        kickers.sort_by(|a, b| b.cmp(a));
        kickers.truncate(3);
        return Some((rank, kickers));
    }
    None
}

struct Player {
    name: String,
    hand: Vec<Card>,
    chips: u32,
    current_bet: u32,
    folded: bool,
    ai: Option<Box<dyn PokerAI>>,  // None for human players
}

struct Game {
    deck: Deck,
    players: Vec<Player>,
    community_cards: Vec<Card>,
    pot: u32,
    current_bet: u32,
    dealer_position: usize,
}

impl Game {
    fn new(players_config: Vec<(String, bool)>) -> Self {  // (name, is_ai)
        let mut deck = Deck::new();
        deck.shuffle();

        let players = players_config
            .into_iter()
            .map(|(name, is_ai)| Player {
                name,
                hand: Vec::new(),
                chips: 1000,
                current_bet: 0,
                folded: false,
                ai: if is_ai {
                    Some(Box::new(BasicPokerAI))
                } else {
                    None
                },
            })
            .collect();

        Self {
            deck,
            players,
            community_cards: Vec::new(),
            pot: 0,
            current_bet: 0,
            dealer_position: 0,
        }
    }
    fn deal_hole_cards(&mut self) {
        for player in &mut self.players {
            player.hand.push(self.deck.deal().unwrap());
            player.hand.push(self.deck.deal().unwrap());
        }
    }
    fn betting_round(&mut self) {
        let mut round_complete = false;
        let mut last_raise_pos = None;

        while !round_complete {
            for i in 0..self.players.len() {
                let idx = (self.dealer_position + i + 1) % self.players.len();
                
                // Skip if player has folded or is all-in
                if self.players[idx].folded || self.players[idx].chips == 0 {
                    continue;
                }

                // Check if we've completed the round
                if let Some(last_raise) = last_raise_pos {
                    if idx == last_raise {
                        round_complete = true;
                        break;
                    }
                }

                let game_state = GameState {
                    current_hand: self.players[idx].hand.clone(),
                    community_cards: self.community_cards.clone(),
                    pot: self.pot,
                    current_bet: self.current_bet,
                    player_chips: self.players[idx].chips,
                    round: self.current_round(),
                };

                let action = if let Some(ref ai) = self.players[idx].ai {
                    let decision = ai.decide_action(&game_state);
                    println!("\n{} is thinking...", self.players[idx].name);
                    std::thread::sleep(std::time::Duration::from_secs(1));
                    decision.action
                } else {
                    self.get_human_action(&self.players[idx])
                };

                // Process the action
                match action {
                    Action::Fold => {
                        self.players[idx].folded = true;
                        println!("{} folds.", self.players[idx].name);
                    }
                    Action::Call => {
                        let call_amount = self.current_bet - self.players[idx].current_bet;
                        self.players[idx].chips -= call_amount;
                        self.players[idx].current_bet += call_amount;
                        self.pot += call_amount;
                        println!("{} calls ${}", self.players[idx].name, call_amount);
                    }
                    Action::Raise(amount) => {
                        let total_bet = self.current_bet - self.players[idx].current_bet + amount;
                        self.players[idx].chips -= total_bet;
                        self.players[idx].current_bet += total_bet;
                        self.pot += total_bet;
                        self.current_bet += amount;
                        last_raise_pos = Some(idx);
                        println!("{} raise by ${}", self.players[idx].name, amount);
                    }
                }
            }
        }

        // Reset players' current bets after the round
        for player in &mut self.players {
            player.current_bet = 0;
        }
        self.current_bet = 0;
    }

    fn current_round(&self) -> BettingRound {
        match self.community_cards.len() {
            0 => BettingRound::PreFlop,
            3 => BettingRound::Flop,
            4 => BettingRound::Turn,
            5 => BettingRound::River,
            _ => panic!("Invalid number of community cards"),
        }
    }

    fn get_human_action(&self, player: &Player) -> Action {
        println!("\n{}'s turn:", player.name);
        println!("Your hand:");
        for card in &player.hand {
            println!("{:?} of {:?}", card.rank, card.suit);
        }
        println!("Current bet is: ${}", self.current_bet);
        println!("Your chips: ${}", player.chips);
        println!("Your current bet: ${}", player.current_bet);
        println!("Pot: ${}", self.pot);

        loop {
            println!("Choose an action: (F)old, (C)all, (R)aise");
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            match input.trim().to_lowercase().chars().next() {
                Some('f') => return Action::Fold,
                Some('c') => return Action::Call,
                Some('r') => {
                    println!("Enter raise amount:");
                    let mut amount_str = String::new();
                    io::stdin().read_line(&mut amount_str).unwrap();
                    if let Ok(amount) = amount_str.trim().parse::<u32>() {
                        if amount > 0 && amount <= player.chips {
                            return Action::Raise(amount);
                        }
                    }
                    println!("Invalid amount. You have ${} chips.", player.chips);
                }
                _ => println!("Invalid action."),
            }
        }
    }

    fn deal_flop(&mut self) {
        // Burn a card
        self.deck.deal();
        // Deal three community cards
        for _ in 0..3 {
            self.community_cards.push(self.deck.deal().unwrap());
        }
    }

    fn deal_turn(&mut self) {
        // Burn a card
        self.deck.deal();
        // Deal one community card
        self.community_cards.push(self.deck.deal().unwrap());
    }

    fn deal_river(&mut self) {
        // Burn a card
        self.deck.deal();
        // Deal one community card
        self.community_cards.push(self.deck.deal().unwrap());
    }

    fn showdown(&self) {
        let mut best_rank = None;
        let mut winner = None;

        for player in &self.players {
            if player.folded {
                continue;
            }

            let all_cards = [player.hand.clone(), self.community_cards.clone()].concat();

            let player_best_hand = all_cards
                .iter()
                .combinations(5)
                .map(|cards| evaluate_hand(&cards.iter().cloned().cloned().collect::<Vec<_>>()))
                .max()
                .unwrap();

            println!("\n{}'s best hand: {:?}", player.name, player_best_hand);

            match best_rank {
                None => {
                    best_rank = Some(player_best_hand);
                    winner = Some(player.name.clone());
                }
                Some(ref rank) if player_best_hand > *rank => {
                    best_rank = Some(player_best_hand);
                    winner = Some(player.name.clone());
                }
                _ => {}
            }
        }

        if let Some(winner_name) = winner {
            println!("\n{} wins the pot of {} chips!", winner_name, self.pot);
        } else {
            println!("\nNo winner.");
        }
    }
}
#[derive(Debug, Clone, Copy)]
struct Card {
    rank: Rank,
    suit: Suit,
}

struct Deck {
    cards: Vec<Card>,
}

impl Deck {
    fn new() -> Self {
        let mut cards = Vec::with_capacity(52);
        for &suit in &[Suit::Hearts, Suit::Diamonds, Suit::Clubs, Suit::Spades] {
            for &rank in &[
                Rank::Two,
                Rank::Three,
                Rank::Four,
                Rank::Five,
                Rank::Six,
                Rank::Seven,
                Rank::Eight,
                Rank::Nine,
                Rank::Ten,
                Rank::Jack,
                Rank::Queen,
                Rank::King,
                Rank::Ace,
            ] {
                cards.push(Card { rank, suit });
            }
        }
        Self { cards }
    }

    fn shuffle(&mut self) {
        let mut rng = thread_rng();
        self.cards.shuffle(&mut rng);
    }

    fn deal(&mut self) -> Option<Card> {
        self.cards.pop()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandRank {
    // Order matters: higher variants are considered better hands
    RoyalFlush,
    StraightFlush(Rank),           // Highest card in the straight
    FourOfAKind(Rank, Rank),       // Rank of the four, then kicker
    FullHouse(Rank, Rank),         // Rank of the three, then the pair
    Flush(Vec<Rank>),              // Ranks of the flush cards
    Straight(Rank),                // Highest card in the straight
    ThreeOfAKind(Rank, Vec<Rank>), // Rank of the three, then kickers
    TwoPair(Rank, Rank, Rank),     // High pair, low pair, then kicker
    OnePair(Rank, Vec<Rank>),      // Rank of the pair, then kickers
    HighCard(Vec<Rank>),           // Ranks of the cards
}

fn main() {
    let players_config = vec![
        ("You".to_string(), false),
        ("AI Bob".to_string(), true),
        ("AI Charlie".to_string(), true),
        ("AI Dave".to_string(), true),
    ];
    
    let mut game = Game::new(players_config);

    println!("\nWelcome to Rust Poker!");
    println!("You start with $1000 chips. Good luck!");

    game.deal_hole_cards();

    println!("\n-- Pre-Flop Betting Round --");
    game.betting_round();

    game.deal_flop();
    println!("\nCommunity cards after the flop:");
    for card in &game.community_cards {
        println!("{:?} of {:?}", card.rank, card.suit);
    }
    println!("\n-- Post-Flop Betting Round --");
    game.betting_round();

    game.deal_turn();
    println!("\nCommunity cards after the turn:");
    for card in &game.community_cards {
        println!("{:?} of {:?}", card.rank, card.suit);
    }
    println!("\n-- Post-Turn Betting Round --");
    game.betting_round();

    game.deal_river();
    println!("\nCommunity cards after the river:");
    for card in &game.community_cards {
        println!("{:?} of {:?}", card.rank, card.suit);
    }
    println!("\n-- Final Betting Round --");
    game.betting_round();

    game.showdown();
}
