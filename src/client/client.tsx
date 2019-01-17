// InfoMark - a platform for managing courses with
//            distributing exercise sheets and testing exercise submissions
// Copyright (C) 2019  ComputerGraphics Tuebingen
// Authors: Patrick Wieschollek
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// TODO: export this to its own git-repo at some point

import axios from 'axios';


// const http = axios.create ({
//   baseURL: process.env.VUE_APP_ROOT_API,
//   timeout: 1000,
//   headers: {'Content-Type': 'application/json'},
// });





export default class InfomarkClient {

  private url_ :string;
  private token_ :string;
  private REST :any;

  /**
   * @param  {REST endpoint url}
   * @param  {JWT token}
   * @return {[type]}
   *
   * Example:
   *
   *  client = new InfomarkClient(url, "")
   *
   *  client.ping()
   *   .catch(function (error) {
   *     if (error.response) {
   *       console.log(error.response.data);
   *       console.log(error.response.status);
   *       console.log(error.response.headers);
   *     }
   *   });
   */
  constructor(url :string, token :string) {
    this.url_ = url + "/api/v1" || "";
    this.token_ = token;

    this.REST = axios.create({
      baseURL: this.url_,
      timeout: 1000,
      headers: {'Authorization': "bearer " + this.token_}
    });



    this.REST.interceptors.request.use (
      (config :any) => {
        if (this.token_ != ""){
          config.headers.Authorization = `Bearer ${this.token_}`;
        }
        return config;
      },
      (error :any) => {
        return Promise.reject (error);
      }
    );


  }

  /**
   * same as login but setups the client
   * NOTE: we probably want to handle this in the app itself
   */
  authenticate(email :string, password :string) {
    this.REST.post("/login", {
      email: email,
      password: password,
    })
    .then((response :any) => {
      this.token_= response.token;
    })
    .catch((error :any) => {})
  }

  /**
   * Returns the health status of the api backend.
   */
  ping() {
    return this.REST.get("/ping");
  }

  /**
   * Returns the user's personal JWT token_.
   */
  login(email :string, password :string) {
    return this.REST.post("/login", {
      email: email,
      password: password,
    });
  }

  // users related REST endpoints
  users_index(){ return this.REST.get("/users"); }
  users_create(user :any){return this.REST.post("/users", user); }
  users_get(id :number){return this.REST.post("/users" + id); }
  users_update(id :number, user :any){return this.REST.patch("/users/" + id, user); }
  users_delete(id :number){ return this.REST.delete("/users/" + id); }
}
