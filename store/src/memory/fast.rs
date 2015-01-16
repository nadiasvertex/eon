struct Element<T> {
    values:        Vec<T>,
    markers:       Vec<bool>,
    levels:        Vec<T>, 
    current_level: i32,
    base_row_id:   i64, 
    initialized:   bool,

}

impl<T> Element<T> {
    fn put (&self, row_id: i64, value: T) {
        if !self.initialized {
            self.initialized = true;
            self.base_row_id = row_id;
        }

        let row_index = row_id - self.base_row_id;
        let required_size = (1 << (self.current_level + 1));

        if row_index < self.values.len() {
            self.values[row_index] = value;
        } else {
            self.values.push(value);
        }

    }
}

